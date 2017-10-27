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
package com.waz.zclient.views.e2ee

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.ImageView
import com.waz.threading.Threading
import com.waz.utils.returning
import com.waz.zclient.ViewHelper
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.ui.R

class ShieldView(context: Context, attrs: AttributeSet, defStyleAttr: Int) extends ImageView(context, attrs, defStyleAttr) with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  val verified = Option(attrs).fold(false) { attrs =>
    val a = context.getTheme.obtainStyledAttributes(attrs, R.styleable.ShieldView, 0, 0)
    returning( a.getBoolean(R.styleable.ShieldView_shieldVerified, false) ) { _ => a.recycle() }
  }

  setImageResource(if (verified) R.drawable.shield_full else R.drawable.shield_half)

  // TODO: check and migrate the way the visibility is set in ParticipantHeaderFragment and SingleParticipantFragment
  inject[ConversationController].currentConvIsVerified.onUi {
    case true  => setVisibility(View.VISIBLE)
    case false => setVisibility(View.GONE)
  }
}
