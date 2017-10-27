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
package com.waz.zclient.views

import android.content.{Context, DialogInterface, Intent}
import android.os.{Build, Bundle}
import android.provider.MediaStore
import android.support.annotation.Nullable
import android.support.v4.app.ActivityCompat
import android.support.v7.widget.{ActionMenuView, Toolbar}
import android.text.TextUtils
import android.text.format.Formatter
import android.view._
import android.view.animation.Animation
import android.widget.{AbsListView, FrameLayout, TextView, Toast}
import com.waz.zclient.{FragmentHelper, R}
import com.waz.zclient.pages.BaseFragment
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api._
import com.waz.api.impl.ContentUriAssetForUpload
import com.waz.model.{AssetId, ConvId, ConversationData, MessageData}
import com.waz.model.ConversationData.ConversationType
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{EventStreamWithAuxSignal, Signal}
import com.waz.utils.returningF
import com.waz.utils.wrappers.URI
import com.waz.zclient.Intents.ShowDevicesIntent
import com.waz.zclient.camera.controllers.GlobalCameraController
import com.waz.zclient.controllers.accentcolor.AccentColorObserver
import com.waz.zclient.controllers.confirmation.{ConfirmationCallback, ConfirmationRequest, IConfirmationController}
import com.waz.zclient.controllers.{ThemeController, UserAccountsController}
import com.waz.zclient.controllers.currentfocus.IFocusController
import com.waz.zclient.controllers.drawing.IDrawingController
import com.waz.zclient.controllers.giphy.GiphyObserver
import com.waz.zclient.controllers.globallayout.KeyboardVisibilityObserver
import com.waz.zclient.controllers.navigation.{NavigationControllerObserver, Page, PagerControllerObserver}
import com.waz.zclient.controllers.orientation.OrientationControllerObserver
import com.waz.zclient.controllers.permission.RequestPermissionsObserver
import com.waz.zclient.controllers.singleimage.SingleImageObserver
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.conversation.{CollectionController, ConversationController}
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.core.stores.inappnotification.SyncErrorObserver
import com.waz.zclient.cursor.{CursorCallback, CursorView}
import com.waz.zclient.media.SoundController
import com.waz.zclient.messages.MessagesListView
import com.waz.zclient.pages.extendedcursor.ExtendedCursorContainer
import com.waz.zclient.pages.extendedcursor.emoji.EmojiKeyboardLayout
import com.waz.zclient.pages.extendedcursor.ephemeral.EphemeralLayout
import com.waz.zclient.pages.extendedcursor.image.{CursorImagesLayout, ImagePreviewLayout}
import com.waz.zclient.pages.extendedcursor.voicefilter.VoiceFilterLayout
import com.waz.zclient.pages.main.conversation.{AssetIntentsManager, MessageStreamAnimation}
import com.waz.zclient.pages.main.conversationlist.ConversationListAnimation
import com.waz.zclient.pages.main.conversationpager.controller.SlidingPaneObserver
import com.waz.zclient.pages.main.pickuser.controller.IPickUserController
import com.waz.zclient.pages.main.profile.camera.CameraContext
import com.waz.zclient.ui.animation.interpolators.penner.Expo
import com.waz.zclient.ui.audiomessage.AudioMessageRecordingView
import com.waz.zclient.ui.cursor.CursorMenuItem
import com.waz.zclient.ui.utils.KeyboardUtils
import com.waz.zclient.utils.{LayoutSpec, PermissionUtils, SquareOrientation, ViewUtils}
import com.waz.zclient.views.e2ee.ShieldView
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.RichView

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.Future
import scala.collection.JavaConversions._

class ConversationFragment extends BaseFragment[ConversationFragment.Container] with FragmentHelper {
  import Threading.Implicits.Ui
  import ConversationFragment._

  private lazy val convController = inject[ConversationController]
  private lazy val collectionController = inject[CollectionController]

  private val previewShown = Signal(false)
  private lazy val convChange = convController.convChanged.filter { _.to.isDefined }
  private lazy val cancelPreviewOnChange = new EventStreamWithAuxSignal(convChange, previewShown)

  private lazy val draftMap = inject[DraftMap]

  private var assetIntentsManager: Option[AssetIntentsManager] = None
  private val sharingUris = new mutable.ListBuffer[URI]()

  private var loadingIndicatorView: LoadingIndicatorView = _
  private var containerPreview: ViewGroup = _
  private var cursorView: CursorView = _
  private var audioMessageRecordingView: AudioMessageRecordingView = _
  private var extendedCursorContainer: ExtendedCursorContainer = _
  private var toolbarTitle: TextView = _
  private var listView: MessagesListView = _

  private var leftMenu: ActionMenuView = _
  private var toolbar: Toolbar = _

  override def onCreateAnimation(transit: Int, enter: Boolean, nextAnim: Int): Animation = {
    implicit val ctx: Context = getActivity
    if (nextAnim == 0 || Option(getContainer).isEmpty || getControllerFactory.isTornDown) super.onCreateAnimation(transit, enter, nextAnim)
    else if (nextAnim == R.anim.fragment_animation_swap_profile_conversation_tablet_in ||
             nextAnim == R.anim.fragment_animation_swap_profile_conversation_tablet_out) new MessageStreamAnimation(
      enter,
      getInt(R.integer.wire__animation__duration__medium),
      0,
      getOrientationDependentDisplayWidth - getResources.getDimensionPixelSize(R.dimen.framework__sidebar_width)
    )
    else if (getControllerFactory.getPickUserController.isHideWithoutAnimations) new ConversationListAnimation(
      0,
      getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
      enter,
      0,
      0,
      false,
      1f
    )
    else if (enter) new ConversationListAnimation(
      0,
      getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
      enter,
      getInt(R.integer.framework_animation_duration_long),
      getInt(R.integer.framework_animation_duration_medium),
      false,
      1f
    )
    else new ConversationListAnimation(
      0,
      getResources.getDimensionPixelSize(R.dimen.open_new_conversation__thread_list__max_top_distance),
      enter,
      getInt(R.integer.framework_animation_duration_medium),
      0,
      false,
      1f
    )
  }

  override def onCreate(@Nullable savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    assetIntentsManager = Option(new AssetIntentsManager(getActivity, assetIntentsManagerCallback, savedInstanceState))
  }

  override def onCreateView(inflater: LayoutInflater, viewGroup: ViewGroup, savedInstanceState: Bundle): View = {
    inflater.inflate(R.layout.fragment_conversation, viewGroup, false)
  }

  override def onViewCreated(view: View, @Nullable savedInstanceState: Bundle): Unit = {
    super.onViewCreated(view, savedInstanceState)

    if (savedInstanceState != null) previewShown ! savedInstanceState.getBoolean(SAVED_STATE_PREVIEW, false)


    implicit val ctx: Context = getActivity

    findById(R.id.tiv_typing_indicator_view)

    loadingIndicatorView = findById(R.id.lbv__conversation__loading_indicator)
    containerPreview = findById(R.id.fl__conversation_overlay)
    cursorView = findById(R.id.cv__cursor)

    extendedCursorContainer = findById(R.id.ecc__conversation)
    listView = findById(R.id.messages_list_view)

    returningF( findById(R.id.sv__conversation_toolbar__verified_shield) ){ view: ShieldView =>
      view.setVisible(false)
    }

    // Recording audio messages
    audioMessageRecordingView = returningF( findById( R.id.amrv_audio_message_recording) ){ view: AudioMessageRecordingView =>
      view.setVisibility(View.INVISIBLE)
      view.setCallback(audioMessageRecordingCallback)
      view.setDarkTheme(inject[ThemeController].isDarkTheme)
    }

    // invisible footer to scroll over inputfield
    returningF( new FrameLayout(getActivity) ){ footer: FrameLayout =>
      footer.setLayoutParams(
        new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, getResources.getDimensionPixelSize(R.dimen.cursor__list_view_footer__height))
      )
    }

    leftMenu = findById(R.id.conversation_left_menu)

    toolbar = findById(R.id.t_conversation_toolbar)

    if (LayoutSpec.isTablet(getContext) && isInLandscape(getContext)) toolbar.setNavigationIcon(null)

    toolbarTitle = ViewUtils.getView(toolbar, R.id.tv__conversation_toolbar__title).asInstanceOf[TextView]
    convController.currentConvName.onUi { updateTitle }


    cancelPreviewOnChange.onUi {
      case (change, Some(true)) if !change.noChange => imagePreviewCallback.onCancelPreview()
      case _ =>
    }

    convController.currentConv.onUi {
      case conv if conv.isActive =>
        inflateCollectionIcon()
        convController.isGroup(conv).foreach { isGroup =>
          toolbar.getMenu.clear()
          toolbar.inflateMenu(if (isGroup) R.menu.conversation_header_menu_audio else R.menu.conversation_header_menu_video)
        }(Threading.Ui)
      case _ =>
    }

    convChange.onUi {
      case ConversationChange(from, Some(to), requester) =>
        CancellableFuture.delay(getResources.getInteger(R.integer.framework_animation_duration_short).millis).map { _ =>
          convController.loadConv(to).map {
            case Some(toConv) =>
              from.foreach{ id => draftMap.set(id, cursorView.getText.trim) }
              if (toConv.convType != ConversationType.WaitForConnection) updateConv(from, toConv)
            case None =>
          }
        }

        // Saving factories since this fragment may be re-created before the runnable is done,
        // but we still want runnable to work.
        val storeFactory = Option(getStoreFactory)
        val controllerFactory = Option(getControllerFactory)
        // TODO: Remove when call issue is resolved with https://wearezeta.atlassian.net/browse/CM-645
        // And also why do we use the ConversationFragment to start a call from somewhere else....
        CancellableFuture.delay(1000.millis).map { _ =>
          (storeFactory, controllerFactory, requester) match {
            case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_VIDEO_CALL) if !sf.isTornDown && !cf.isTornDown =>
              cf.getCallingController.startCall(true)
            case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_CALL) if !sf.isTornDown && !cf.isTornDown =>
              cf.getCallingController.startCall(false)
            case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_CALL) if !sf.isTornDown && !cf.isTornDown =>
              cf.getCameraController.openCamera(CameraContext.MESSAGE)
            case _ =>
          }
        }

      case _ =>
    }
  }

  override def onStart(): Unit = {
    super.onStart()

    toolbar.setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit =
        getControllerFactory.getConversationScreenController.showParticipants(toolbar, false)
    })

    toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
      override def onMenuItemClick(item: MenuItem): Boolean = item.getItemId match {
        case R.id.action_audio_call =>
          getControllerFactory.getCallingController.startCall(false)
          cursorView.closeEditMessage(false)
          true
        case R.id.action_video_call =>
          getControllerFactory.getCallingController.startCall(true)
          cursorView.closeEditMessage(false)
          true
        case _ => false
      }
    })

    toolbar.setNavigationOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = {
        cursorView.closeEditMessage(false)
        getActivity.onBackPressed()
        KeyboardUtils.closeKeyboardIfShown(getActivity)
      }
    })

    leftMenu.setOnMenuItemClickListener(new ActionMenuView.OnMenuItemClickListener() {
      override def onMenuItemClick(item: MenuItem): Boolean = item.getItemId match {
        case R.id.action_collection =>
          collectionController.openCollection()
          true
        case _ => false
      }
    })

    getControllerFactory.getGlobalLayoutController.addKeyboardHeightObserver(extendedCursorContainer)
    getControllerFactory.getGlobalLayoutController.addKeyboardVisibilityObserver(extendedCursorContainer)
    extendedCursorContainer.setCallback(extendedCursorContainerCallback)

    getControllerFactory.getRequestPermissionsController.addObserver(requestPermissionsObserver)
    getControllerFactory.getOrientationController.addOrientationControllerObserver(orientationControllerObserver)
    cursorView.setCallback(cursorCallback)

    draftMap.withCurrentDraft { draftText => if (!TextUtils.isEmpty(draftText)) cursorView.setText(draftText) }

    getControllerFactory.getNavigationController.addNavigationControllerObserver(navigationControllerObserver)
    getControllerFactory.getNavigationController.addPagerControllerObserver(pagerControllerObserver)
    getControllerFactory.getGiphyController.addObserver(giphyObserver)
    getControllerFactory.getSingleImageController.addSingleImageObserver(singleImageObserver)
    getControllerFactory.getAccentColorController.addAccentColorObserver(accentColorObserver)
    getControllerFactory.getGlobalLayoutController.addKeyboardVisibilityObserver(keyboardVisibilityObserver)
    getStoreFactory.inAppNotificationStore.addInAppNotificationObserver(syncErrorObserver)
    getControllerFactory.getSlidingPaneController.addObserver(slidingPaneObserver)
  }

  private def updateTitle(text: String): Unit = if (toolbarTitle != null) toolbarTitle.setText(text)

  override def onResume(): Unit = {
    super.onResume()
    audioMessageRecordingView.setVisibility(View.INVISIBLE)
  }

  override def onSaveInstanceState(outState: Bundle): Unit = {
    super.onSaveInstanceState(outState)
    assetIntentsManager.foreach { _.onSaveInstanceState(outState) }
    previewShown.head.foreach { isShown => outState.putBoolean(SAVED_STATE_PREVIEW, isShown) }
  }

  override def onPause(): Unit = {
    super.onPause()
    KeyboardUtils.hideKeyboard(getActivity)
    hideAudioMessageRecording()
  }

  override def onStop(): Unit = {
    extendedCursorContainer.close(true)
    extendedCursorContainer.setCallback(null)
    cursorView.setCallback(null)

    toolbar.setOnClickListener(null)
    toolbar.setOnMenuItemClickListener(null)
    toolbar.setNavigationOnClickListener(null)
    audioMessageRecordingView.setCallback(null)
    leftMenu.setOnMenuItemClickListener(null)

    getControllerFactory.getGlobalLayoutController.removeKeyboardHeightObserver(extendedCursorContainer)
    getControllerFactory.getGlobalLayoutController.removeKeyboardVisibilityObserver(extendedCursorContainer)

    getControllerFactory.getOrientationController.removeOrientationControllerObserver(orientationControllerObserver)
    getControllerFactory.getGiphyController.removeObserver(giphyObserver)
    getControllerFactory.getSingleImageController.removeSingleImageObserver(singleImageObserver)

    if (!cursorView.isEditingMessage) draftMap.setCurrent(cursorView.getText.trim)

    getStoreFactory.inAppNotificationStore.removeInAppNotificationObserver(syncErrorObserver)
    getControllerFactory.getGlobalLayoutController.removeKeyboardVisibilityObserver(keyboardVisibilityObserver)
    getControllerFactory.getAccentColorController.removeAccentColorObserver(accentColorObserver)

    getControllerFactory.getSlidingPaneController.removeObserver(slidingPaneObserver)

    getControllerFactory.getConversationScreenController.setConversationStreamUiReady(false)

    getControllerFactory.getNavigationController.removePagerControllerObserver(pagerControllerObserver)
    getControllerFactory.getNavigationController.removeNavigationControllerObserver(navigationControllerObserver)
    getControllerFactory.getRequestPermissionsController.removeObserver(requestPermissionsObserver)

    super.onStop()
  }

  private def updateConv(fromId: Option[ConvId], toConv: ConversationData): Unit = {
    KeyboardUtils.hideKeyboard(getActivity)
    loadingIndicatorView.hide()
    cursorView.enableMessageWriting()

    fromId.filter(_ != toConv.id).foreach { id =>
      getControllerFactory.getConversationScreenController.setConversationStreamUiReady(false)

      cursorView.setVisible(toConv.isActive)
      draftMap.get(toConv.id).map { draftText =>
        cursorView.setText(draftText)
        cursorView.setConversation()
      }
      hideAudioMessageRecording()
    }

    getControllerFactory.getConversationScreenController.setSingleConversation(toConv.convType == IConversation.Type.ONE_TO_ONE)
    // TODO: ConversationScreenController should listen to this signal and do it itself
    extendedCursorContainer.close(true)
  }

  private def inflateCollectionIcon(): Unit = {
    leftMenu.getMenu.clear()

    val searchInProgress = collectionController.contentSearchQuery.currentValue("").get.originalString.nonEmpty

    getActivity.getMenuInflater.inflate(
      if (searchInProgress) R.menu.conversation_header_menu_collection_searching
      else R.menu.conversation_header_menu_collection,
      leftMenu.getMenu
    )
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit =
    assetIntentsManager.foreach { _.onActivityResult(requestCode, resultCode, data) }

  private lazy val imagePreviewCallback = new ImagePreviewLayout.Callback {
    override def onCancelPreview(): Unit = {
      previewShown ! false
      getControllerFactory.getNavigationController.setPagerEnabled(true)
      containerPreview
        .animate
        .translationY(getView.getMeasuredHeight)
        .setDuration(getResources.getInteger(R.integer.animation_duration_medium))
        .setInterpolator(new Expo.EaseIn)
        .withEndAction(new Runnable() {
          override def run(): Unit = if (containerPreview != null) containerPreview.removeAllViews()
        })
    }

    override def onSketchOnPreviewPicture(imageAsset: ImageAsset, source: ImagePreviewLayout.Source, method: IDrawingController.DrawingMethod): Unit = {
      getControllerFactory.getDrawingController.showDrawing(imageAsset, IDrawingController.DrawingDestination.CAMERA_PREVIEW_VIEW, method)
      extendedCursorContainer.close(true)
    }

    override def onSendPictureFromPreview(imageAsset: ImageAsset, source: ImagePreviewLayout.Source): Unit = imageAsset match {
      case a: com.waz.api.impl.ImageAsset => convController.currentConv.head.map { conv =>
        convController.sendMessage(conv.id, a)
        extendedCursorContainer.close(true)
        onCancelPreview()
      }
      case _ =>
    }

  }

  private def errorHandler = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = answer.ok()
  }

  private lazy val audioMessageRecordingCallback = new AudioMessageRecordingView.Callback {

    override def onPreviewedAudioMessage(): Unit = {}
    override def onSendAudioMessage(audioAssetForUpload: AudioAssetForUpload, appliedAudioEffect: AudioEffect, sentWithQuickAction: Boolean): Unit = audioAssetForUpload match {
      case a: com.waz.api.impl.AudioAssetForUpload =>
        convController.currentConvId.head.map { convId =>
          convController.sendMessage(convId, a, errorHandler)
          hideAudioMessageRecording()
        }
      case _ =>
    }

    override def onCancelledAudioMessageRecording(): Unit = hideAudioMessageRecording()

    override def onStartedRecordingAudioMessage(): Unit = getControllerFactory.getGlobalLayoutController.keepScreenAwake()
  }

  private def sendVideo(uri: URI): Unit = {
    convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), assetErrorHandlerVideo)
    getControllerFactory.getNavigationController.setRightPage(Page.MESSAGE_STREAM, TAG)
    extendedCursorContainer.close(true)
  }

  private def sendImage(uri: URI): Unit = ImageAssetFactory.getImageAsset(uri) match {
    case a: com.waz.api.impl.ImageAsset => convController.sendMessage(a)
    case _ =>
  }

  private val assetErrorHandler = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = Option(getActivity) match {
      case None => answer.ok()
      case Some(activity) =>
        val dialog = ViewUtils.showAlertDialog(
          activity,
          R.string.asset_upload_warning__large_file__title,
          R.string.asset_upload_warning__large_file__message_default,
          R.string.asset_upload_warning__large_file__button_accept,
          R.string.asset_upload_warning__large_file__button_cancel,
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.ok() },
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.cancel() }
        )
        dialog.setCancelable(false)
        if (sizeInBytes > 0)
          dialog.setMessage(getString(R.string.asset_upload_warning__large_file__message, Formatter.formatFileSize(getContext, sizeInBytes)))
    }
  }

  private val assetErrorHandlerVideo = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = Option(getActivity) match {
      case None => answer.ok()
      case Some(activity) =>
        val dialog = ViewUtils.showAlertDialog(
          activity,
          R.string.asset_upload_warning__large_file__title,
          R.string.asset_upload_warning__large_file__message_default,
          R.string.asset_upload_warning__large_file__button_accept,
          R.string.asset_upload_warning__large_file__button_cancel,
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.ok() },
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.cancel() }
      )
      dialog.setCancelable(false)
      if (sizeInBytes > 0) dialog.setMessage(getString(R.string.asset_upload_warning__large_file__message__video))
    }
  }

  private val assetIntentsManagerCallback = new AssetIntentsManager.Callback {
    override def onDataReceived(intentType: AssetIntentsManager.IntentType, uri: URI): Unit = intentType match {
      case AssetIntentsManager.IntentType.FILE_SHARING =>
        sharingUris.clear()
        if (PermissionUtils.hasSelfPermissions(getActivity, FILE_SHARING_PERMISSION:_*)) convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), errorHandler)
        else {
          sharingUris += uri
          ActivityCompat.requestPermissions(getActivity, FILE_SHARING_PERMISSION, FILE_SHARING_PERMISSION_REQUEST_ID)
        }
      case AssetIntentsManager.IntentType.GALLERY =>
        showImagePreview(ImageAssetFactory.getImageAsset(uri), ImagePreviewLayout.Source.DEVICE_GALLERY)
      case AssetIntentsManager.IntentType.VIDEO_CURSOR_BUTTON =>
        sendVideo(uri)
      case AssetIntentsManager.IntentType.VIDEO =>
        sendVideo(uri)
      case AssetIntentsManager.IntentType.CAMERA =>
        sendImage(uri)
        extendedCursorContainer.close(true)
    }

    override def openIntent(intent: Intent, intentType: AssetIntentsManager.IntentType): Unit = {
      if (MediaStore.ACTION_VIDEO_CAPTURE.equals(intent.getAction) &&
        extendedCursorContainer.getType == ExtendedCursorContainer.Type.IMAGES &&
        extendedCursorContainer.isExpanded) {
        // Close keyboard camera before requesting external camera for recording video
        extendedCursorContainer.close(true)
      }
      startActivityForResult(intent, intentType.requestCode)
      getActivity.overridePendingTransition(R.anim.camera_in, R.anim.camera_out)
    }

    override def onPermissionFailed(`type`: AssetIntentsManager.IntentType): Unit = {}

    override def onFailed(`type`: AssetIntentsManager.IntentType): Unit = {}

    override def onCanceled(`type`: AssetIntentsManager.IntentType): Unit = {}
  }

  private val extendedCursorContainerCallback = new ExtendedCursorContainer.Callback {
    override def onExtendedCursorClosed(lastType: ExtendedCursorContainer.Type): Unit = {
      cursorView.onExtendedCursorClosed()

      if (lastType == ExtendedCursorContainer.Type.EPHEMERAL)
        convController.currentConv.head.map { conv =>
          if (conv.ephemeral != EphemeralExpiration.NONE)
            getControllerFactory.getUserPreferencesController.setLastEphemeralValue(conv.ephemeral.milliseconds)
        }

      getControllerFactory.getGlobalLayoutController.resetScreenAwakeState()
    }
  }

  private val requestPermissionsObserver = new RequestPermissionsObserver() {
    override def onRequestPermissionsResult(requestCode: Int, grantResults: Array[Int]) =
      if (!assetIntentsManager.fold(true){_.onRequestPermissionsResult(requestCode, grantResults)}) requestCode match {
        case OPEN_EXTENDED_CURSOR_IMAGES =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) openExtendedCursor(ExtendedCursorContainer.Type.IMAGES)
        case CAMERA_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) {
            val takeVideoIntent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE)
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.JELLY_BEAN_MR1) takeVideoIntent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 0)
            startActivityForResult(takeVideoIntent, REQUEST_VIDEO_CAPTURE)
          }
          else cameraPermissionsFail
        case FILE_SHARING_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) {
            sharingUris.foreach { uri => convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), assetErrorHandler) }
            sharingUris.clear()
          }
          else ViewUtils.showAlertDialog(
            getActivity,
            R.string.asset_upload_error__not_found__title,
            R.string.asset_upload_error__not_found__message,
            R.string.asset_upload_error__not_found__button,
            null,
            true
          )
        case AUDIO_PERMISSION_REQUEST_ID =>
          // No actions required if permission is granted
          // TODO: https://wearezeta.atlassian.net/browse/AN-4027 Show information dialog if permission is not granted
        case AUDIO_FILTER_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) openExtendedCursor(ExtendedCursorContainer.Type.VOICE_FILTER_RECORDING)
          else Toast.makeText(getActivity, R.string.audio_message_error__missing_audio_permissions, Toast.LENGTH_SHORT).show()
        case _ =>
          verbose(s"onRequestPermissionsResult(unknown)")
      }
    }

  private def cameraPermissionsFail =
    Toast.makeText(getActivity, R.string.video_message_error__missing_camera_permissions, Toast.LENGTH_SHORT).show()

  private def openExtendedCursor(cursorType: ExtendedCursorContainer.Type): Unit = cursorType match {
      case ExtendedCursorContainer.Type.NONE =>
      case ExtendedCursorContainer.Type.EMOJIS =>
        extendedCursorContainer.openEmojis(getControllerFactory.getUserPreferencesController.getRecentEmojis, getControllerFactory.getUserPreferencesController.getUnsupportedEmojis, emojiKeyboardLayoutCallback)
      case ExtendedCursorContainer.Type.EPHEMERAL => convController.currentConv.head.map { conv =>
        extendedCursorContainer.openEphemeral(ephemeralLayoutCallback, conv.ephemeral)
      }
      case ExtendedCursorContainer.Type.VOICE_FILTER_RECORDING =>
        extendedCursorContainer.openVoiceFilter(voiceFilterLayoutCallback)
      case ExtendedCursorContainer.Type.IMAGES =>
        extendedCursorContainer.openCursorImages(cursorImageLayoutCallback)
      case _ =>
        verbose(s"openExtendedCursor(unknown)")
    }


  private val emojiKeyboardLayoutCallback = new EmojiKeyboardLayout.Callback {
    override def onEmojiSelected(emoji: LogTag) = {
      cursorView.insertText(emoji)
      getControllerFactory.getUserPreferencesController.addRecentEmoji(emoji)
    }
  }

  private val ephemeralLayoutCallback = new EphemeralLayout.Callback {
    override def onEphemeralExpirationSelected(expiration: EphemeralExpiration, close: Boolean) = {
      if (close) extendedCursorContainer.close(false)
      convController.setEphemeralExpiration(expiration)
    }
  }

  private val voiceFilterLayoutCallback = new VoiceFilterLayout.Callback {
    override def onAudioMessageRecordingStarted(): Unit = {
      getControllerFactory.getGlobalLayoutController.keepScreenAwake()
    }

    override def onCancel(): Unit = extendedCursorContainer.close(false)

    override def sendRecording(audioAssetForUpload: AudioAssetForUpload, appliedAudioEffect: AudioEffect): Unit = {
      audioAssetForUpload match {
        case a: com.waz.api.impl.AudioAssetForUpload => convController.sendMessage(a, errorHandler)
        case _ =>
      }

      hideAudioMessageRecording()
      extendedCursorContainer.close(true)
    }
  }

  private val cursorImageLayoutCallback = new CursorImagesLayout.Callback {
    override def openCamera(): Unit = getControllerFactory.getCameraController.openCamera(CameraContext.MESSAGE)

    override def openVideo(): Unit = assetIntentsManager.foreach { _.maybeCaptureVideo(getActivity, AssetIntentsManager.IntentType.VIDEO) }

    override def onGalleryPictureSelected(asset: ImageAsset): Unit = {
      previewShown ! true
      showImagePreview(asset, ImagePreviewLayout.Source.IN_APP_GALLERY)
    }

    override def openGallery(): Unit = assetIntentsManager.foreach { _.openGallery() }

    override def onPictureTaken(imageAsset: ImageAsset): Unit = showImagePreview(imageAsset, ImagePreviewLayout.Source.CAMERA)
  }

  private lazy val inLandscape = Signal(Option.empty[Boolean])

  private val orientationControllerObserver = new  OrientationControllerObserver {
    override def onOrientationHasChanged(squareOrientation: SquareOrientation): Unit = inLandscape.head.foreach { oldInLandscape =>
      implicit val ctx: Context = getActivity
      val newInLandscape = isInLandscape
      oldInLandscape match {
        case Some(landscape) if landscape != newInLandscape =>
          val conversationListVisible = getControllerFactory.getNavigationController.getCurrentPage eq Page.CONVERSATION_LIST
          if (newInLandscape && !conversationListVisible)
            CancellableFuture.delayed(getInt(R.integer.framework_animation_duration_short).millis){
              Option(getActivity).foreach(_.onBackPressed())
            }
        case _ =>
      }
      inLandscape ! Some(newInLandscape)
    }
  }

  private val cursorCallback = new CursorCallback {
    override def onMotionEventFromCursorButton(cursorMenuItem: CursorMenuItem, motionEvent: MotionEvent): Unit =
      if (cursorMenuItem == CursorMenuItem.AUDIO_MESSAGE && audioMessageRecordingView.getVisibility == View.VISIBLE)
        audioMessageRecordingView.onMotionEventFromAudioMessageButton(motionEvent)

    override def captureVideo(): Unit = inject[GlobalCameraController].releaseCamera().map { _ =>
      assetIntentsManager.foreach { _.maybeCaptureVideo(getActivity, AssetIntentsManager.IntentType.VIDEO_CURSOR_BUTTON) }
    }

    override def hideExtendedCursor(): Unit = if (extendedCursorContainer.isExpanded) extendedCursorContainer.close(false)

    override def onMessageSent(msg: MessageData): Unit = {
      getStoreFactory.networkStore.doIfHasInternetOrNotifyUser(null)
    }

    override def openExtendedCursor(tpe: ExtendedCursorContainer.Type): Unit = ConversationFragment.this.openExtendedCursor(tpe)

    override def onCursorClicked(): Unit = if (!cursorView.isEditingMessage) listView.scrollToBottom()

    override def onFocusChange(hasFocus: Boolean): Unit = {
      if (hasFocus) getControllerFactory.getFocusController.setFocus(IFocusController.CONVERSATION_CURSOR)
      if (!LayoutSpec.isPhone(getActivity) && getControllerFactory.getPickUserController.isShowingPickUser(IPickUserController.Destination.CONVERSATION_LIST)) {
        // On tablet, apply Page.MESSAGE_STREAM soft input mode when conversation cursor has focus (soft input mode of page gets changed when left startui is open)
        val softInputMode = getControllerFactory.getGlobalLayoutController.getSoftInputModeForPage(if (hasFocus) Page.MESSAGE_STREAM else Page.PICK_USER)
        ViewUtils.setSoftInputMode(getActivity.getWindow, softInputMode, TAG)
      }
    }

    override def openFileSharing(): Unit = assetIntentsManager.foreach { _.openFileSharing() }

    override def onCursorButtonLongPressed(cursorMenuItem: CursorMenuItem): Unit = if (cursorMenuItem == CursorMenuItem.AUDIO_MESSAGE) {
      if (PermissionUtils.hasSelfPermissions(getActivity, AUDIO_PERMISSION:_*)) {
        extendedCursorContainer.close(true)
        if (audioMessageRecordingView.getVisibility != View.VISIBLE) {
          inject[SoundController].shortVibrate()
          audioMessageRecordingView.prepareForRecording()
          audioMessageRecordingView.setVisibility(View.VISIBLE)
        }
      } else ActivityCompat.requestPermissions(getActivity, AUDIO_PERMISSION, AUDIO_PERMISSION_REQUEST_ID)
    }
  }

  private val navigationControllerObserver = new NavigationControllerObserver {
    override def onPageVisible(page: Page): Unit = if (page == Page.MESSAGE_STREAM) {
      inflateCollectionIcon()
      cursorView.enableMessageWriting()
    }
  }

  private val pagerControllerObserver = new PagerControllerObserver {
    override def onPageScrolled(position: Int, positionOffset: Float, positionOffsetPixels: Int): Unit =
      if (positionOffset > 0) extendedCursorContainer.close(true)

    override def onPagerEnabledStateHasChanged(enabled: Boolean): Unit = {}
    override def onPageSelected(position: Int): Unit = {}
    override def onPageScrollStateChanged(state: Int): Unit = {}
  }

  private val giphyObserver = new GiphyObserver {
    override def onSearch(keyword: LogTag): Unit = {}
    override def onRandomSearch(): Unit = {}
    override def onTrendingSearch(): Unit = {}
    override def onCloseGiphy(): Unit = cursorView.setText("")
    override def onCancelGiphy(): Unit = {}
  }

  private val singleImageObserver = new SingleImageObserver {
    override def onShowSingleImage(message: Message): Unit = {}
    override def onShowUserImage(user: User): Unit = {}
    override def onHideSingleImage(): Unit = getControllerFactory.getNavigationController.setRightPage(Page.MESSAGE_STREAM, TAG)
  }

  private val accentColorObserver = new AccentColorObserver {
    override def onAccentColorHasChanged(sender: Any, color: Int): Unit = {
      loadingIndicatorView.setColor(color)
      audioMessageRecordingView.setAccentColor(color)
      extendedCursorContainer.setAccentColor(color)
    }
  }

  private val keyboardVisibilityObserver = new KeyboardVisibilityObserver {
    override def onKeyboardVisibilityChanged(keyboardIsVisible: Boolean, keyboardHeight: Int, currentFocus: View): Unit =
      cursorView.notifyKeyboardVisibilityChanged(keyboardIsVisible, currentFocus)
  }

  private val syncErrorObserver = new SyncErrorObserver {
    override def onSyncError(errorDescription: ErrorsList.ErrorDescription): Unit = errorDescription.getType match {
      case ErrorType.CANNOT_SEND_ASSET_FILE_NOT_FOUND =>
        ViewUtils.showAlertDialog(
          getActivity,
          R.string.asset_upload_error__not_found__title,
          R.string.asset_upload_error__not_found__message,
          R.string.asset_upload_error__not_found__button,
          null,
          true
        )
        errorDescription.dismiss()
      case ErrorType.CANNOT_SEND_ASSET_TOO_LARGE =>
        val maxAllowedSizeInBytes = AssetFactory.getMaxAllowedAssetSizeInBytes
        if (maxAllowedSizeInBytes > 0) {
          val dialog = ViewUtils.showAlertDialog(
            getActivity,
            R.string.asset_upload_error__file_too_large__title,
            R.string.asset_upload_error__file_too_large__message_default,
            R.string.asset_upload_error__file_too_large__button,
            null,
            true
          )
          dialog.setMessage(getString(R.string.asset_upload_error__file_too_large__message, Formatter.formatShortFileSize(getContext, maxAllowedSizeInBytes)))
        }
        errorDescription.dismiss()
      case ErrorType.RECORDING_FAILURE =>
        ViewUtils.showAlertDialog(
          getActivity,
          R.string.audio_message__recording__failure__title,
          R.string.audio_message__recording__failure__message,
          R.string.alert_dialog__confirmation,
          null,
          true
        )
        errorDescription.dismiss()
      case ErrorType.CANNOT_SEND_MESSAGE_TO_UNVERIFIED_CONVERSATION =>
        onErrorCanNotSentMessageToUnverifiedConversation(errorDescription)
    }

    private def onErrorCanNotSentMessageToUnverifiedConversation(errorDescription: ErrorsList.ErrorDescription) =
      if (getControllerFactory.getNavigationController.getCurrentPage != Page.MESSAGE_STREAM) {
        KeyboardUtils.hideKeyboard(getActivity)

        (for {
          self <- inject[UserAccountsController].currentUser.head
          members <- convController.loadMembers(new ConvId(errorDescription.getConversation.getId))
          unverifiedUsers = (members ++ self.map(Seq(_)).getOrElse(Nil)).filter {
            !_.isVerified
          }
          unverifiedDevices <-
          if (unverifiedUsers.size == 1) Future.sequence(unverifiedUsers.map(u => convController.loadClients(u.id).map(_.filter(!_.isVerified)))).map(_.flatten.size)
          else Future.successful(0) // in other cases we don't need this number
        } yield (self, unverifiedUsers, unverifiedDevices)).map { case (self, unverifiedUsers, unverifiedDevices) =>

          val unverifiedNames = unverifiedUsers.map { u => if (self.map(_.id).contains(u.id)) getString(R.string.conversation_degraded_confirmation__header__you) else u.displayName }

          val header =
            if (unverifiedUsers.isEmpty) getResources.getString(R.string.conversation__degraded_confirmation__header__someone)
            else if (unverifiedUsers.size == 1)
              getResources.getQuantityString(R.plurals.conversation__degraded_confirmation__header__single_user, unverifiedDevices, unverifiedNames.head)
            else getString(R.string.conversation__degraded_confirmation__header__multiple_user, unverifiedNames.mkString(","))

          val onlySelfChanged = unverifiedUsers.size == 1 && self.map(_.id).contains(unverifiedUsers.head.id)

          val callback = new ConfirmationCallback {
            override def positiveButtonClicked(checkboxIsSelected: Boolean): Unit = {
              errorDescription.getMessages.toSeq.foreach(_.retry())
              errorDescription.dismiss()
            }

            override def onHideAnimationEnd(confirmed: Boolean, cancelled: Boolean, checkboxIsSelected: Boolean): Unit = if (!confirmed && !cancelled) {
              if (onlySelfChanged) getContext.startActivity(ShowDevicesIntent(getActivity))
              else getControllerFactory.getConversationScreenController.showParticipants(ViewUtils.getView(getActivity, R.id.cursor_menu_item_participant), true)
            }

            override def negativeButtonClicked(): Unit = {}

            override def canceled(): Unit = {}
          }

          val positiveButton = getString(R.string.conversation__degraded_confirmation__positive_action)
          val negativeButton =
            if (onlySelfChanged) getString(R.string.conversation__degraded_confirmation__negative_action_self)
            else getResources.getQuantityString(R.plurals.conversation__degraded_confirmation__negative_action, unverifiedUsers.size)

          val messageCount = Math.max(1, errorDescription.getMessages.toSeq.size)
          val message = getResources.getQuantityString(R.plurals.conversation__degraded_confirmation__message, messageCount)

          val request =
            new ConfirmationRequest.Builder()
              .withHeader(header)
              .withMessage(message)
              .withPositiveButton(positiveButton)
              .withNegativeButton(negativeButton)
              .withConfirmationCallback(callback)
              .withCancelButton()
              .withBackgroundImage(R.drawable.degradation_overlay)
              .withWireTheme(inject[ThemeController].getThemeDependentOptionsTheme)
              .build

          getControllerFactory.getConfirmationController.requestConfirmation(request, IConfirmationController.CONVERSATION)
        }
      }
  }

  private val slidingPaneObserver = new SlidingPaneObserver {
    override def onPanelSlide(panel: View, slideOffset: Float): Unit = {}
    override def onPanelOpened(panel: View): Unit = KeyboardUtils.closeKeyboardIfShown(getActivity)
    override def onPanelClosed(panel: View): Unit = {}
  }

  private def hideAudioMessageRecording(): Unit = if (audioMessageRecordingView.getVisibility == View.VISIBLE) {
    audioMessageRecordingView.reset()
    audioMessageRecordingView.setVisibility(View.INVISIBLE)
    getControllerFactory.getGlobalLayoutController.resetScreenAwakeState()
  }

  private def showImagePreview(asset: ImageAsset, source: ImagePreviewLayout.Source): Unit = {
    val imagePreviewLayout = LayoutInflater.from(getContext).inflate(R.layout.fragment_cursor_images_preview, containerPreview, false).asInstanceOf[ImagePreviewLayout]
    imagePreviewLayout.setImageAsset(asset, source, imagePreviewCallback)
    imagePreviewLayout.setAccentColor(getControllerFactory.getAccentColorController.getAccentColor.getColor)
    convController.currentConv.head.map { conv => imagePreviewLayout.setTitle(conv.displayName) }
    containerPreview.addView(imagePreviewLayout)
    openPreview(containerPreview)
  }

  private def openPreview(containerPreview: View): Unit = {
    previewShown ! true
    getControllerFactory.getNavigationController.setPagerEnabled(false)
    containerPreview.setTranslationY(getView.getMeasuredHeight)
    containerPreview.animate.translationY(0).setDuration(getResources.getInteger(R.integer.animation_duration_medium)).setInterpolator(new Expo.EaseOut)
  }
}

object ConversationFragment {
  val TAG = ConversationFragment.getClass.getName
  val SAVED_STATE_PREVIEW = "SAVED_STATE_PREVIEW"
  val REQUEST_VIDEO_CAPTURE = 911
  val CAMERA_PERMISSION_REQUEST_ID = 21

  val OPEN_EXTENDED_CURSOR_IMAGES = 1254

  val FILE_SHARING_PERMISSION = Array[String](android.Manifest.permission.READ_EXTERNAL_STORAGE)
  val FILE_SHARING_PERMISSION_REQUEST_ID = 179

  val AUDIO_PERMISSION = Array[String](android.Manifest.permission.RECORD_AUDIO)
  val AUDIO_PERMISSION_REQUEST_ID = 864
  val AUDIO_FILTER_PERMISSION_REQUEST_ID = 865

  def apply() = new ConversationFragment

  trait Container {

  }
}
