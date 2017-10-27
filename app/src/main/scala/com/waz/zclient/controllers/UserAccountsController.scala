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
package com.waz.zclient.controllers

import android.content.Context
import com.waz.ZLog.ImplicitTag._
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.{BaseActivity, Injectable, Injector}

import scala.concurrent.Future

class UserAccountsController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  val zms = inject[Signal[ZMessaging]]

  val accounts = Option(ZMessaging.currentAccounts).fold(Signal.const(Seq.empty[AccountData]))(_.loggedInAccounts.map(_.sortBy(acc => (acc.isTeamAccount, acc.id.str))))

  val currentUser = for {
    zms     <- zms
    account <- ZMessaging.currentAccounts.activeAccount
    user    <- account.flatMap(_.userId).fold(Signal.const(Option.empty[UserData]))(accId => zms.usersStorage.signal(accId).map(Some(_)))
  } yield user

  private var _permissions = Set[AccountData.Permission]()

  private var _teamData = Option.empty[TeamData]
  private var _teamId = Option.empty[TeamId]
  private var _teamMembers = Set.empty[UserId]

  val permissions = for {
    zms <- zms
    accountData <- zms.account.accountData
  } yield accountData.selfPermissions

  permissions { p =>
    _permissions = p
  }

  val teamDataSignal = for {
    zms <- zms
    teamData <- zms.teams.selfTeam
  } yield teamData

  teamDataSignal { data => _teamData = data }

  def teamData = _teamData

  val teamMembersSignal = for {
    zms <- zms
    teamMembers <- zms.teams.searchTeamMembers()
  } yield teamMembers.map(_.id)

  teamMembersSignal { members => _teamMembers = members }

  private def unreadCountForConv(conversationData: ConversationData): Int = {
    if (conversationData.archived || conversationData.muted || conversationData.hidden)
      0
    else
      conversationData.unreadCount.total
  }

  val unreadCount = for {
    zmsSet   <- ZMessaging.currentAccounts.zmsInstances
    countMap <- Signal.sequence(zmsSet.map(z => z.convsStorage.convsSignal.map(c => z.accountId -> c.conversations.map(unreadCountForConv).sum)).toSeq:_*)
  } yield countMap.toMap

  def createAndOpenConversation(users: Array[UserId], requester: ConversationChangeRequester,  activity: BaseActivity): Future[Unit] = {
    val createConv = for {
      z <- zms.head
      user <- z.usersStorage.get(z.selfUserId)
      conv <-
        if (users.length == 1 && !isTeamAccount)
          z.convsUi.getOrCreateOneToOneConversation(users.head)
        else
          z.convsUi.createGroupConversation(ConvId(), users, teamId)
    } yield conv

    val conversationController = inject[ConversationController]
    (for {
      conv <- createConv
      Some(loaded) <- conversationController.loadConv(conv.id) // is this necessary?
      _ <- conversationController.selectConv(Some(loaded.id), requester)
    } yield ())(Threading.Ui)
  }

  zms.map(_.teamId) { case teamId => _teamId = teamId }

  def teamId = _teamId
  def isTeamAccount = _teamId.isDefined
  def isTeamMember(userId: UserId) = _teamMembers.contains(userId)

  //TODO should perhaps clean this up a tad
  private def getTeamId(convId: ConvId): Option[TeamId] = zms.currentValue.flatMap(_.convsStorage.conversations.find(_.id == convId).flatMap(_.team))

  private def isPartOfTeam(tId: TeamId): Boolean = teamId match {
    case Some(id) => id == tId
    case _ => false
  }

  def hasCreateConversationPermission: Boolean = !isTeamAccount || _permissions(AccountData.Permission.CreateConversation)
  def hasRemoveConversationMemberPermission(convId: ConvId): Boolean = getTeamId(convId) match {
    case Some(id) => isPartOfTeam(id) && _permissions(AccountData.Permission.RemoveConversationMember)
    case _ => true
  }
  def hasAddConversationMemberPermission(convId: ConvId): Boolean = getTeamId(convId) match {
    case Some(id) => isPartOfTeam(id) && _permissions(AccountData.Permission.AddConversationMember)
    case _ => true
  }

}
