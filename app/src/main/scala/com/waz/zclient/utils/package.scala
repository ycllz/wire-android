/**
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
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
package com.waz.zclient

import android.graphics.LightingColorFilter
import android.graphics.drawable.LayerDrawable

import android.support.v7.preference.Preference
import android.support.v7.preference.Preference.{OnPreferenceChangeListener, OnPreferenceClickListener}
import android.text.{Editable, TextWatcher}
import android.view.View._
import android.view.ViewGroup.LayoutParams
import android.view.ViewGroup.LayoutParams.WRAP_CONTENT
import android.view.{View, ViewGroup}
import android.widget.{SeekBar, TextView}
import com.waz.utils.returning
import com.waz.zclient.ui.views.OnDoubleClickListener

package object utils {

  case class Offset(l: Int, t: Int, r: Int, b: Int)
  object Offset {
    val Empty = Offset(0, 0, 0, 0)
  }

  implicit class RichView(val view: View) extends AnyVal {

    def setVisible(isVisible: Boolean): Unit = view.setVisibility(if (isVisible) VISIBLE else GONE)

    def setGone(isGone: Boolean): Unit = view.setVisibility(if (isGone) GONE else VISIBLE)

    def isVisible = view.getVisibility == VISIBLE

    def setMarginTop(m: Int) = {
      view.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].topMargin = m
      view.requestLayout()
    }

    def setMargin(r: Offset): Unit = setMargin(r.l, r.t, r.r, r.b)

    def setMargin(l: Int, t: Int, r: Int, b: Int): Unit = {
      val lp = view.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams]
      lp.leftMargin = l
      lp.topMargin = t
      lp.rightMargin = r
      lp.bottomMargin = b
      view.requestLayout()
    }

    //TODO improve this so that multiple click listeners can be set from different places at once
    //TODO could also handle a set of views?
    def onClick(f: => Unit): Unit = view.setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = f
    })

    def onClick(onSingleClickArg: => Unit, onDoubleClickArg: => Unit): Unit = view.setOnClickListener(new OnDoubleClickListener {
      override def onSingleClick(): Unit = onSingleClickArg
      override def onDoubleClick(): Unit = onDoubleClickArg
    })

    def onLongClick(f: => Boolean): Unit = view.setOnLongClickListener(new OnLongClickListener {
      override def onLongClick(v: View): Boolean = f
    })

    def setWidthAndHeight(w: Option[Int] = None, h: Option[Int] = None) = {
      view.setLayoutParams(Option(view.getLayoutParams) match {
        case Some(p) =>
          p.width = w.getOrElse(p.width)
          p.height = h.getOrElse(p.height)
          p
        case _ =>
          new LayoutParams(w.getOrElse(WRAP_CONTENT), h.getOrElse(WRAP_CONTENT))
      })
    }
    def setWidth(w: Int): Unit = setWidthAndHeight(w = Some(w))
    def setHeight(h: Int): Unit = setWidthAndHeight(h = Some(h))
  }

  implicit class RichTextView(val textView: TextView) extends AnyVal {
    def addTextListener(callback: String => Unit): TextWatcher = {
      returning(new TextWatcher {
        override def beforeTextChanged(s: CharSequence, start: Int, count: Int, after: Int) = { }
        override def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int) = callback(s.toString)
        override def afterTextChanged(s: Editable) = {}
      }){ textView.addTextChangedListener }
    }
  }

  implicit class RichPreference(val pref: Preference) extends AnyVal {
    def onClick(f: => Unit) = pref.setOnPreferenceClickListener(new OnPreferenceClickListener {
      override def onPreferenceClick(preference: Preference): Boolean = {
        f
        true
      }
    })

    /**
      * @param f True to update the state of the Preference with the new value.
      */
    def onChange(f: Any => Boolean) = pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener {
      override def onPreferenceChange(preference: Preference, o: Any): Boolean = {
        f(o)
        true
      }
    })
  }

  implicit class RichSeekBar(val bar: SeekBar) extends AnyVal {
    def setColor(color: Int): Unit = {
      val progressDrawable = Option(bar.getProgressDrawable).map {
        case d: LayerDrawable => Option(d.findDrawableByLayerId(android.R.id.progress)).getOrElse(d)
        case d => d
      }
      val thumbDrawable = Option(bar.getThumb)
      val filter = new LightingColorFilter(0xFF000000, color)
      Seq(progressDrawable, thumbDrawable).foreach(_.foreach(_.setColorFilter(filter)))
    }
  }

}
