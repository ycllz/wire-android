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
package com.waz.zclient.fragments

import android.content.Intent
import android.os.Bundle
import android.support.v7.widget.{LinearLayoutManager, RecyclerView}
import android.view.animation.Animation
import android.view.{LayoutInflater, View, ViewGroup}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.otr.Client
import com.waz.model.{AccountId, ConversationData}
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.zclient.adapters.ConversationListAdapter
import com.waz.zclient.controllers.UserAccountsController
import com.waz.zclient.controllers.global.AccentColorController

import com.waz.zclient.conversation.ConversationController

import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.pages.BaseFragment
import com.waz.zclient.pages.main.conversation.controller.IConversationScreenController
import com.waz.zclient.pages.main.conversationlist.ConversationListAnimation
import com.waz.zclient.pages.main.conversationlist.views.ListActionsView
import com.waz.zclient.pages.main.conversationlist.views.ListActionsView.Callback
import com.waz.zclient.pages.main.conversationlist.views.listview.SwipeListView
import com.waz.zclient.pages.main.pickuser.controller.IPickUserController
import com.waz.zclient.preferences.PreferencesActivity
import com.waz.zclient.ui.text.TypefaceTextView
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.{RichView, ViewUtils}
import com.waz.zclient.views.conversationlist.{ArchiveTopToolbar, ConversationListTopToolbar, NormalTopToolbar}

import com.waz.zclient.{FragmentHelper, OnBackPressedListener, R}

abstract class ConversationListFragment extends BaseFragment[ConversationListFragment.Container] with FragmentHelper {

  implicit lazy val context = getContext

  val layoutId: Int
  lazy val userAccountsController = inject[UserAccountsController]

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle) =
    inflater.inflate(layoutId, container, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    val topToolbar = ViewUtils.getView(view, R.id.conversation_list_top_toolbar).asInstanceOf[ConversationListTopToolbar]

    val adapter = returning(new ConversationListAdapter(getContext)) { a =>
      a.setMaxAlpha(getResourceFloat(R.dimen.list__swipe_max_alpha))
      a.currentMode.on(Threading.Ui) { mode =>
        topToolbar.title.setText(mode.nameId)
      }
    }

    val conversationListView = returning(ViewUtils.getView(view, R.id.conversation_list_view).asInstanceOf[SwipeListView]) { rv =>
      rv.setLayoutManager(new LinearLayoutManager(getContext))
      rv.setAdapter(adapter)
      rv.setAllowSwipeAway(true)
      rv.setOverScrollMode(View.OVER_SCROLL_NEVER)
      rv.addOnScrollListener(new RecyclerView.OnScrollListener {
        override def onScrolled(recyclerView: RecyclerView, dx: Int, dy: Int) = {
          topToolbar.setScrolledToTop(!recyclerView.canScrollVertically(-1))
        }
      })
    }
    userAccountsController.currentUser.on(Threading.Ui) { _ =>
      conversationListView.scrollToPosition(0)
    }

    adapter.onConversationClick { handleItemClick }
    adapter.onConversationLongClick { handleItemLongClick(_, conversationListView) }

    init(view, adapter)
  }

  def init(view: View, adapter: ConversationListAdapter): Unit

  private def handleItemClick(conversationData: ConversationData): Unit = {
    val conversationChangeRequester =
      if (conversationData.archived)
        ConversationChangeRequester.CONVERSATION_LIST_UNARCHIVED_CONVERSATION
      else
        ConversationChangeRequester.CONVERSATION_LIST

    inject[ConversationController].selectConv(Option(conversationData.id), conversationChangeRequester)
  }

  private def handleItemLongClick(conversationData: ConversationData, anchorView: View): Unit = {
    if (conversationData.convType != ConversationType.Group &&
        conversationData.convType != ConversationType.OneToOne &&
        conversationData.convType != ConversationType.WaitForConnection) {
      return
    }

    getControllerFactory.getConversationScreenController.showConversationMenu(IConversationScreenController.CONVERSATION_LIST_LONG_PRESS, conversationData.id, anchorView)
  }

  override def onCreateAnimation(transit: Int, enter: Boolean, nextAnim: Int): Animation = {
    if (nextAnim == 0 || getContainer == null || getControllerFactory.isTornDown)
      return super.onCreateAnimation(transit, enter, nextAnim)

    if (getControllerFactory.getPickUserController.isHideWithoutAnimations)
      return new ConversationListAnimation(
        0,
        getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
        enter,
        0,
        0,
        false,
        1f)

    if (enter)
      return new ConversationListAnimation(
        0,
        getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
        enter,
        getResources.getInteger(R.integer.framework_animation_duration_long),
        getResources.getInteger(R.integer.framework_animation_duration_medium),
        false,
        1f)

    new ConversationListAnimation(
      0,
      getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
      enter,
      getResources.getInteger(R.integer.framework_animation_duration_medium),
      0,
      false,
      1f)
  }
}

object ArchiveListFragment{
  val TAG = ArchiveListFragment.getClass.getSimpleName
}
class ArchiveListFragment extends ConversationListFragment with OnBackPressedListener {

  override val layoutId = R.layout.fragment_archive_list

  override def init(view: View, adapter: ConversationListAdapter): Unit ={
    val topToolbar = ViewUtils.getView(view, R.id.conversation_list_top_toolbar).asInstanceOf[ArchiveTopToolbar]
    adapter.currentMode ! ConversationListAdapter.Archive
    topToolbar.onRightButtonClick{ _ => Option(getContainer).foreach(_.closeArchive()) }
  }

  override def onBackPressed() = {
    Option(getContainer).foreach(_.closeArchive())
    true
  }
}

object NormalConversationListFragment{
  val TAG = NormalConversationListFragment.getClass.getSimpleName
}
class NormalConversationFragment extends ConversationListFragment {

  override val layoutId = R.layout.fragment_conversation_list

  lazy val zms = inject[Signal[ZMessaging]]
  lazy val accentColor = inject[AccentColorController].accentColor
  lazy val incomingClients = for{
    z       <- zms
    color   <- accentColor
    acc     <- z.account.accountData
    clients <- acc.clientId.fold(Signal.empty[Seq[Client]])(aid => z.otrClientsStorage.incomingClientsSignal(z.selfUserId, aid))
  } yield (color.getColor(), clients)

  private lazy val unreadCount = (for {
    Some(accountId) <- ZMessaging.currentAccounts.activeAccountPref.signal
    count  <- userAccountsController.unreadCount.map(_.filterNot(_._1 == accountId).values.sum)
  } yield count).orElse(Signal.const(0))

  lazy val hasConversationsAndArchive = for {
    z <- zms
    convs <- z.convsStorage.convsSignal
  } yield {
    (convs.conversations.exists(c => !c.archived && !c.hidden),
    convs.conversations.exists(c => c.archived && !c.hidden))
  }

  def topToolbar = Option(getView).map(p => findById[NormalTopToolbar](p, R.id.conversation_list_top_toolbar))
  def listActionsView = Option(getView).map(p => findById[ListActionsView](p, R.id.lav__conversation_list_actions))
  def conversationListView = Option(getView).map(p => findById[SwipeListView](p, R.id.conversation_list_view))
  def loadingListView = Option(getView).map(p => findById[View](p, R.id.conversation_list_loading_indicator))

  private var adapter = Option.empty[ConversationListAdapter]
  private val waitingAccount = Signal[Option[AccountId]](None)

  val loading = for {
    Some(waitingAcc) <- waitingAccount
    adapterAccount <- adapter.fold(Signal.empty[AccountId])(_.conversationListData.map(_._1))
  } yield waitingAcc != adapterAccount

  loading.onUi {
    case true => showLoading()
    case false =>
      hideLoading()
      waitingAccount ! None
  }

  override def init(view: View, adapter: ConversationListAdapter): Unit = {
    this.adapter = Some(adapter)

    val noConvsTitle = ViewUtils.getView(view, R.id.conversation_list_empty_title).asInstanceOf[TypefaceTextView]
    val noConvsSubtitle = ViewUtils.getView(view, R.id.conversation_list_empty_subtitle).asInstanceOf[TypefaceTextView]

    conversationListView.foreach(_.addOnScrollListener(new RecyclerView.OnScrollListener {
      override def onScrolled(recyclerView: RecyclerView, dx: Int, dy: Int) = {
        listActionsView.foreach(_.setScrolledToBottom(!recyclerView.canScrollVertically(1)))
      }
    }))

    adapter.currentMode ! ConversationListAdapter.Normal
    hasConversationsAndArchive.on(Threading.Ui) {
      case (false, true) =>
        noConvsTitle.setText(R.string.all_archived__header)
        noConvsTitle.setVisible(true)
        noConvsSubtitle.setVisible(false)
        listActionsView.foreach(_.setArchiveEnabled(true))
      case (false, false) =>
        noConvsTitle.setText(R.string.no_conversation_in_list__header)
        noConvsTitle.setVisible(true)
        noConvsSubtitle.setVisible(true)
        listActionsView.foreach(_.setArchiveEnabled(false))
      case (_, archive) =>
        noConvsTitle.setVisible(false)
        noConvsSubtitle.setVisible(false)
        listActionsView.foreach(_.setArchiveEnabled(archive))
    }

    topToolbar.foreach(_.onRightButtonClick { _ => getActivity.startActivityForResult(PreferencesActivity.getDefaultIntent(getContext), PreferencesActivity.SwitchAccountCode) })

    Signal(unreadCount, incomingClients).on(Threading.Ui) {
      case (count, (color, clients)) =>
        topToolbar.foreach(_.setIndicatorVisible(clients.nonEmpty || count > 0))
        topToolbar.foreach(_.setIndicatorColor(color))
    }

    listActionsView.foreach(_.setCallback(new Callback {
      override def onAvatarPress() = {
        getControllerFactory.getPickUserController.showPickUser(IPickUserController.Destination.CONVERSATION_LIST, null)
      }

      override def onArchivePress() = {
        Option(getContainer).foreach(_.showArchive())
      }
    }))
  }

  private def showLoading(): Unit = {
    conversationListView.foreach(_.setVisibility(View.INVISIBLE))
    loadingListView.foreach(_.setVisibility(View.VISIBLE))
    loadingListView.foreach(_.setAlpha(1f))
    listActionsView.foreach(_.setAlpha(0.5f))
    topToolbar.foreach(_.setLoading(true))
  }

  private def hideLoading(): Unit = {
    conversationListView.foreach { clv =>
      if (!clv.isVisible) {
        clv.setVisibility(View.VISIBLE)
        clv.setAlpha(0f)
        clv.animate().alpha(1f).setDuration(500)
        listActionsView.foreach(_.animate().alpha(1f).setDuration(500))
        loadingListView.foreach(_.animate().alpha(0f).setDuration(500).withEndAction(new Runnable {
          override def run() = loadingListView.foreach(_.setVisibility(View.GONE))
        }))
      }
    }
    topToolbar.foreach(_.setLoading(false))
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    if (requestCode == PreferencesActivity.SwitchAccountCode && data != null) {
      showLoading()
      waitingAccount ! Some(AccountId(data.getStringExtra(PreferencesActivity.SwitchAccountExtra)))
    }
  }
}

object ConversationListFragment {
  trait Container {
    def showArchive(): Unit
    def closeArchive(): Unit
  }

  def newNormalInstance(): ConversationListFragment = {
    new NormalConversationFragment()
  }

  def newArchiveInstance(): ConversationListFragment = {
    new ArchiveListFragment()
  }
}
