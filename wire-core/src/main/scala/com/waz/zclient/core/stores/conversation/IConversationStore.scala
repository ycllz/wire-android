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
package com.waz.zclient.core.stores.conversation

import com.waz.api.AssetForUpload
import com.waz.api.AudioAssetForUpload
import com.waz.api.IConversation
import com.waz.api.ImageAsset
import com.waz.api.MessageContent
import com.waz.api.SyncState
import com.waz.api.User
import com.waz.model.ConvId
import com.waz.zclient.core.stores.IStore



trait IConversationStore extends IStore {

  /**
   * adds an observer on this store
   * @param conversationStoreObserver
   */
  def addConversationStoreObserver(conversationStoreObserver: ConversationStoreObserver): Unit

  def addConversationStoreObserverAndUpdate(conversationStoreObserver: ConversationStoreObserver): Unit

  /**
   * removes an observer on this store
   * @param conversationStoreObserver
   */
  def removeConversationStoreObserver(conversationStoreObserver: ConversationStoreObserver): Unit

  /**
   * For use when archiving a conversation - you need to set a new current conversation
   *
   * @return IConversation - if the below conversation is not archived this will be returned,
   * otherwise the conversation above
   */
  def nextConversation(convId: ConvId): Option[ConvId]

  def getConversation(conversationId: String): IConversation

  def numberOfActiveConversations: Int

  protected def conversationSyncingState: SyncState
}
