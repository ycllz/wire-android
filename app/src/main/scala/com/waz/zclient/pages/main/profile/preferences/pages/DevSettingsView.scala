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
package com.waz.zclient.pages.main.profile.preferences.pages

import android.app.AlertDialog
import android.content.{Context, DialogInterface}
import android.os.Bundle
import android.util.AttributeSet
import android.view.View
import android.widget.LinearLayout
import com.waz.content.GlobalPreferences._
import com.waz.content.UserPreferences.LastStableNotification
import com.waz.model.Uid
import com.waz.service.ZMessaging
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.zclient.pages.main.profile.preferences.views.{SwitchPreference, TextButton}
import com.waz.zclient.utils.BackStackKey
import com.waz.zclient.{R, ViewHelper}

trait DevSettingsView

class DevSettingsViewImpl(context: Context, attrs: AttributeSet, style: Int) extends LinearLayout(context, attrs, style) with DevSettingsView with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)

  def this(context: Context) = this(context, null, 0)

  inflate(R.layout.preferences_dev_layout)

  val autoAnswerSwitch = returning(findById[SwitchPreference](R.id.preferences_dev_auto_answer)) { v =>
    v.setPreference(AutoAnswerCallPrefKey, global = true)
  }

  val cloudMessagingSwitch = returning(findById[SwitchPreference](R.id.preferences_dev_gcm)) { v =>
    v.setPreference(PushEnabledKey, global = true)
  }

  val webSocketForegroundServiceSwitch = returning(findById[SwitchPreference](R.id.preferences_dev_websocket_service)) { v =>
    v.setPreference(WsForegroundKey, global = true)
  }

  val randomLastIdButton = findById[TextButton](R.id.preferences_dev_generate_random_lastid)

  randomLastIdButton.onClickEvent { _ =>
    val randomUid = Uid()

    new AlertDialog.Builder(context)
      .setTitle("Random new value for LastStableNotification")
      .setMessage(s"Sets LastStableNotification to $randomUid")
      .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
        override def onClick(dialog: DialogInterface, which: Int): Unit = {
          val zms = inject[Signal[ZMessaging]]
          zms.map(_.userPrefs.preference(LastStableNotification)).onUi {
            _ := Some(randomUid)
          }
        }
      })
      .setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
        override def onClick(dialog: DialogInterface, which: Int): Unit = {}
      })
      .setIcon(android.R.drawable.ic_dialog_alert).show
  }
}

case class DevSettingsBackStackKey(args: Bundle = new Bundle()) extends BackStackKey(args) {
  override def nameId: Int = R.string.pref_developer_screen_title

  override def layoutId = R.layout.preferences_dev

  override def onViewAttached(v: View) = {}

  override def onViewDetached() = {}
}
