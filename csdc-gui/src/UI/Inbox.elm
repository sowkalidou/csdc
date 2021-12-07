module UI.Inbox exposing (..)

import Types exposing (..)
import UI.BoxMessage as BoxMessage
import UI.BoxReply as BoxReply

import Html exposing (Html)

type InboxId
  = MessageMemberId (Id (Message NewMember))
  | ReplyMemberId (Id (Reply NewMember))
  | MessageSubpartId (Id (Message NewSubpart))
  | ReplySubpartId (Id (Reply NewSubpart))

view : Inbox -> List (Html InboxId)
view inbox =
  let
    toBoxMessageMember =
      Html.map MessageMemberId << BoxMessage.view False
    toBoxReplyMember =
      Html.map ReplyMemberId << BoxReply.view False
    toBoxMessageSubpart =
      Html.map MessageSubpartId << BoxMessage.view False
    toBoxReplySubpart =
      Html.map ReplySubpartId << BoxReply.view False
  in
    List.map toBoxMessageMember inbox.messageMember ++
    List.map toBoxReplyMember inbox.replyMember ++
    List.map toBoxMessageSubpart inbox.messageSubpart ++
    List.map toBoxReplySubpart inbox.replySubpart
