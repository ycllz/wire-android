/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.conversation

import android.content.Context
import com.waz.api.{EphemeralExpiration, IConversation, Verification}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{EventContext, EventStream, Signal, SourceStream}
import com.waz.zclient.controllers.UserAccountsController
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.utils.Callback
import com.waz.zclient.{BaseActivity, Injectable, Injector}
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api
import com.waz.api.MessageContent.Asset.ErrorHandler
import com.waz.api.impl.{AssetForUpload, ImageAsset}
import com.waz.model.otr.Client
import com.waz.utils.{Serialized, returning}
import com.waz.zclient.core.stores.IStoreFactory
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import com.waz.utils._

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private lazy val convStore = inject[IStoreFactory].conversationStore
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var lastConvId = Option.empty[ConvId]

  val currentConvId: Signal[ConvId] = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(convId) => convId }
  val currentConvOpt: Signal[Option[ConversationData]] = currentConvId.flatMap { conversationData } // updates on every change of the conversation data, not only on switching
  val currentConv: Signal[ConversationData] = currentConvOpt.collect { case Some(conv) => conv }

  val convChanged: SourceStream[ConversationChange] = EventStream[ConversationChange]()

  def conversationData(convId: ConvId): Signal[Option[ConversationData]] = storage.flatMap(_.optSignal(convId))

  val currentConvType: Signal[ConversationType] = currentConv.map(_.convType)
  val currentConvName: Signal[String] = currentConv.map { _.displayName } // the name of the current conversation can be edited (without switching)
  val currentConvIsVerified: Signal[Boolean] = currentConv.map { _.verified == Verification.VERIFIED }
  val currentConvIsGroup: Signal[Boolean] = currentConv.flatMap { conv => Signal.future(isGroup(conv)) }

  for {
    z <- zms
    convId <- currentConvId
  } yield z.conversations.forceNameUpdate(convId)

  def withCurrentConv(f: (ConversationData) => Unit): Future[Unit] = currentConv.head.map(f)

  // this should be the only UI entry point to change conv in SE
  def selectConv(convId: Option[ConvId], requester: ConversationChangeRequester): Future[Unit] = convId match {
    case None => Future.successful({})
    case Some(_) =>
      stats.head.flatMap(_.selectConversation(convId)).map { _ =>
        convChanged ! ConversationChange(from = lastConvId, to = convId, requester = requester)
        lastConvId = convId
      }
  }

  def selectConv(id: ConvId, requester: ConversationChangeRequester): Future[Unit] = selectConv(Some(id), requester)

  def loadConv(convId: ConvId): Future[Option[ConversationData]] = storage.head.flatMap(_.get(convId))

  def createAndOpenConv(users: Array[UserId], requester: ConversationChangeRequester, activity: BaseActivity): Future[Unit] =
    for {
      z <- zms.head
      user <- z.usersStorage.get(z.selfUserId)
      conv <- if (users.length == 1 && !userAccounts.isTeamAccount) z.convsUi.getOrCreateOneToOneConversation(users.head)
              else z.convsUi.createGroupConversation(ConvId(), users, userAccounts.teamId)
    } yield selectConv(Option(conv.id), ConversationChangeRequester.START_CONVERSATION)

  def getOrCreateConv(userId: UserId): Future[ConversationData] = for {
    z <- zms.head
    conv <- z.convsUi.getOrCreateOneToOneConversation(userId)
  } yield {
    conv
  }

  def isGroup(conv: ConversationData): Future[Boolean] = {
    if (conv.team.isEmpty) Future.successful(conv.convType == ConversationType.Group)
    else zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map { _.size > 2 } // maybe this could be changed to activeMembers
  }

  def setEphemeralExpiration(expiration: EphemeralExpiration): Future[Unit] = for {
    z <- zms.head
    id <- currentConvId.head
    _ <- z.convsUi.setEphemeral(id, expiration)
  } yield ()

  def loadMembers(convId: ConvId): Future[Seq[UserData]] = for {
    z <- zms.head
    userIds <- z.membersStorage.activeMembers(convId).head
    users <- z.users.getUsers(userIds.toSeq)
  } yield users

  def loadClients(userId: UserId): Future[Seq[Client]] = zms.head.flatMap(_.otrClientsStorage.getClients(userId)) // TODO: move to SE maybe?

  def sendMessage(audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(convId: ConvId, audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(text: String): Future[Unit] = for {
    z <- zms.head
    convId <- currentConvId.head
  } yield z.convsUi.sendMessage(convId, text)
  def sendMessage(imageAsset: com.waz.api.ImageAsset): Future[Unit] = imageAsset match { // TODO: remove when not used anymore
    case a: com.waz.api.impl.ImageAsset => currentConvId.head.map { convId => sendMessage(convId, a) }
    case _ => Future.successful({})
  }
  def sendMessage(imageAsset: ImageAsset): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, imageAsset) }
  def sendMessage(convId: ConvId, imageAsset: ImageAsset): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, imageAsset) }
  def sendMessage(location: api.MessageContent.Location): Future[Unit] = for {
    z <- zms.head
    convId <- currentConvId.head
  } yield z.convsUi.sendMessage(convId, location)

  def setCurrentConvName(name: String): Future[Unit] = for {
    z <- zms.head
    convId <- currentConvId.head
  } yield z.convsUi.setConversationName(convId, name)

  def addMembers(id: ConvId, users: Set[UserId]): Future[Unit] = zms.head.map { _.convsUi.addConversationMembers(id, users.toSeq) }

  def removeMember(user: UserId): Future[Unit] = for {
    z <- zms.head
    id <- currentConvId.head
  } yield z.convsUi.removeConversationMember(id, user)

  def leave(convId: ConvId): CancellableFuture[Option[ConversationData]] =
    returning (Serialized("Conversations", convId) { CancellableFuture.lift( zms.head.flatMap { _.convsUi.leaveConversation(convId) } ) } ) { _ =>
      currentConvId.head.map { id => if (id == convId) setCurrentConversationToNext(ConversationChangeRequester.LEAVE_CONVERSATION) }
    }

  def setCurrentConversationToNext(requester: ConversationChangeRequester): Future[Unit] =
    currentConvId.head
      .map { id => convStore.nextConversation(id) }
      .map { convId =>selectConv(convId, requester) }

  def archive(convId: ConvId, archive: Boolean): Unit = {
    zms.head.map { _.convsUi.setConversationArchived(convId, archive) }
    currentConvId.head.map { id => if (id == convId) CancellableFuture.delayed(ConversationController.ARCHIVE_DELAY){
      if (!archive) selectConv(convId, ConversationChangeRequester.CONVERSATION_LIST_UNARCHIVED_CONVERSATION)
      else setCurrentConversationToNext(ConversationChangeRequester.ARCHIVED_RESULT)
    }}
  }

  def clear(id: ConvId): CancellableFuture[Option[ConversationData]] = Serialized("Conversations", id) { CancellableFuture.lift( zms.head.flatMap { _.convsUi.clearConversation(id) } ) }

  def setMuted(id: ConvId, muted: Boolean): Future[Unit] = zms.head.map { _.convsUi.setConversationMuted(id, muted) }

  def delete(id: ConvId, alsoLeave: Boolean): CancellableFuture[Option[ConversationData]] =
    if (alsoLeave) leave(id).flatMap(_ => clear(id)) else clear(id)

  def knock(id: ConvId): Unit = zms(_.convsUi.knock(id))

  def createGroupConversation(users: Seq[UserId], localId: ConvId = ConvId()): Future[ConversationData] =
    zms.head.flatMap { _.convsUi.createGroupConversation(localId, users) }

  // TODO: remove when not used anymore

  def iConv(id: ConvId): IConversation = convStore.getConversation(id.str)
  def iCurrentConv: IConversation = currentConvId.currentValue.map(iConv).orNull

  def withCurrentConv(callback: Callback[ConversationData]): Unit = currentConv.head.foreach( callback.callback )
  def withCurrentConvName(callback: Callback[String]): Unit = currentConvName.head.foreach(callback.callback)
  def withCurrentConvType(callback: Callback[IConversation.Type]): Unit = currentConvType.head.foreach(callback.callback)

  def getCurrentConvId: ConvId = currentConvId.currentValue.orNull
  def onConvChanged(callback: Callback[ConversationChange]): Unit =  convChanged.onUi { callback.callback }
  def withConvLoaded(convId: ConvId, callback: Callback[ConversationData]): Unit =
    loadConv(convId).foreach {
      case Some(data) => callback.callback(data)
      case None =>
    }

  def withMembers(convId: ConvId, callback: Callback[java.util.Collection[UserData]]): Unit =
    loadMembers(convId).foreach { users =>callback.callback(users.asJavaCollection) }
  def withCurrentConvMembers(callback: Callback[java.util.Collection[UserData]]): Unit = currentConvId.head.foreach { id => withMembers(id, callback) }
  def addMembers(id: ConvId, users: java.util.List[UserId]): Unit = addMembers(id, users.asScala.toSet)

  def createGroupConversation(users: java.util.List[UserId], conversationChangerSender: ConversationChangeRequester): Unit =
    createGroupConversation(users.asScala).map { data =>
      selectConv(Some(data.id),
        if (conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_CALL &&
          conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_VIDEO_CALL &&
          conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_CAMERA) ConversationChangeRequester.START_CONVERSATION
        else conversationChangerSender
      )
    }

  object messages {

    val ActivityTimeout = 3.seconds

    /**
      * Currently focused message.
      * There is only one focused message, switched by tapping.
      */
    val focused = Signal(Option.empty[MessageId])

    /**
      * Tracks last focused message together with last action time.
      * It's not cleared when message is unfocused, and toggleFocus takes timeout into account.
      * This is used to decide if timestamp view should be shown in footer when message has likes.
      */
    val lastActive = Signal((MessageId.Empty, Instant.EPOCH)) // message showing status info

    currentConv.onChanged { _ => clear() }

    def clear() = {
      focused ! None
      lastActive ! (MessageId.Empty, Instant.EPOCH)
    }

    def isFocused(id: MessageId): Boolean = focused.currentValue.flatten.contains(id)

    /**
      * Switches current msg focus state to/from given msg.
      */
    def toggleFocused(id: MessageId) = {
      verbose(s"toggleFocused($id)")
      focused mutate {
        case Some(`id`) => None
        case _ => Some(id)
      }
      lastActive.mutate {
        case (`id`, t) if !ActivityTimeout.elapsedSince(t) => (id, Instant.now - ActivityTimeout)
        case _ => (id, Instant.now)
      }
    }
  }
}

object ConversationController {
  val ARCHIVE_DELAY = 500.millis

  case class ConversationChange(from: Option[ConvId], to: Option[ConvId], requester: ConversationChangeRequester) {
    def toConvId: ConvId = to.orNull // TODO: remove when not used anymore
    lazy val noChange: Boolean = from == to
  }

  val emptyImageAsset: com.waz.api.ImageAsset = ImageAsset.Empty.asInstanceOf[com.waz.api.ImageAsset]

  def getOtherParticipantForOneToOneConv(conv: ConversationData): UserId = {
    if (conv != ConversationData.Empty &&
        conv.convType != IConversation.Type.ONE_TO_ONE &&
        conv.convType != IConversation.Type.WAIT_FOR_CONNECTION &&
        conv.convType != IConversation.Type.INCOMING_CONNECTION)
      error(s"unexpected call, most likely UI error", new UnsupportedOperationException(s"Can't get other participant for: ${conv.convType} conversation"))
    UserId(conv.id.str) // one-to-one conversation has the same id as the other user, so we can access it directly
  }

}
