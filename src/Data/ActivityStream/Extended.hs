{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Data.ActivityStream.Extended where

import Import
import Data.ActivityStream.Core

-- begin Activity Types
data AcceptPart = AcceptPart
data AddPart = AddPart
data AnnouncePart = AnnouncePart
data ArrivePart = ArrivePart
data BlockPart = BlockPart
data CreatePart = CreatePart
data DeletePart = DeletePart
data DislikePart = DislikePart
data FlagPart = FlagPart
data FollowPart = FollowPart
data IgnorePart = IgnorePart
data InvitePart = InvitePart
data JoinPart = JoinPart
data LeavePart = LeavePart
data LikePart = LikePart
data ListenPart = ListenPart
data MovePart = MovePart
data OfferPart = OfferPart
data QuestionPart = QuestionPart
  { oneOf :: Maybe Oneof
  , anyOf :: Maybe Anyof
  , closed :: Maybe Closed
  }
data RejectPart = RejectPart
data ReadPart = ReadPart
data RemovePart = RemovePart
data TentativeRejectPart = TentativeRejectPart
data TentativeAcceptPart = TentativeAcceptPart
data TravelPart = TravelPart
data UndoPart = UndoPart
data UpdatePart = UpdatePart
data ViewPart = ViewPart

data Accept = Accept ObjectPart ActivityPart AcceptPart
data TentativeAccept = TentativeAccept ObjectPart ActivityPart AcceptPart TentativeAcceptPart
data Add = Add ObjectPart ActivityPart AddPart
data Arrive = Arrive ObjectPart ActivityPart IntransitiveActivityPart ArrivePart
data Create = Create ObjectPart ActivityPart CreatePart
data Delete = Delete ObjectPart ActivityPart DeletePart
data Follow = Follow ObjectPart ActivityPart FollowPart
data Ignore = Ignore ObjectPart ActivityPart IgnorePart
data Join = Join ObjectPart ActivityPart JoinPart
data Leave = Leave ObjectPart ActivityPart LeavePart
data Like = Like ObjectPart ActivityPart LikePart
data Offer = Offer ObjectPart ActivityPart OfferPart
data Invite = Invite ObjectPart ActivityPart OfferPart InvitePart
data Reject = Reject ObjectPart ActivityPart RejectPart
data TentativeReject = TentativeReject ObjectPart ActivityPart RejectPart TentativeRejectPart
data Remove = Remove ObjectPart ActivityPart RemovePart
data Undo = Undo ObjectPart ActivityPart UndoPart
data Update = Update ObjectPart ActivityPart UpdatePart
data View = View ObjectPart ActivityPart ViewPart
data Listen = Listen ObjectPart ActivityPart ListenPart
data Read = Read ObjectPart ActivityPart ReadPart
data Move = Move ObjectPart ActivityPart MovePart
data Travel = Travel ObjectPart ActivityPart IntransitiveActivityPart TravelPart
data Announce = Announce
data Block = Block ObjectPart ActivityPart IgnorePart BlockPart
data Flag = Flag ObjectPart ActivityPart FlagPart
data Dislike = Dislike ObjectPart ActivityPart DislikePart
data Question = Question ObjectPart ActivityPart IntransitiveActivityPart QuestionPart
-- end Activity Types

-- begin Actor Types
data ApplicationPart = ApplicationPart
data GroupPart = GroupPart
data OrganizationPart = OrganizationPart
data PersonPart = PersonPart
data ServicePart = ServicePart

data Application = Application ObjectPart ApplicationPart
data Group = Group ObjectPart GroupPart
data Organization = Organization ObjectPart OrganizationPart
data Person = Person ObjectPart PersonPart
data Service = Service ObjectPart ServicePart
-- end Actor Types

-- begin Object and Link Types
data ArticlePart = ArticlePart
data AudioPart = AudioPart
data DocumentPart = DocumentPart
data EventPart = EventPart
data ImagePart = ImagePart
data NotePart = NotePart
data PagePart = PagePart
data PlacePart = PlacePart
  { accuracy :: Maybe Accuracy
  , altitude :: Maybe Altitude
  , latitude :: Maybe Latitude
  , longitude :: Maybe Longitude
  , radius :: Maybe Radius
  , units :: Maybe Units
  }
data ProfilePart = ProfilePart
  { describes :: Maybe Describes }
data RelationshipPart = RelationshipPart
  { subject :: Maybe Subject
  , object :: Maybe ObjectProp
  , relationship :: Maybe Relationship}
data TombstonePart = TombstonePart
  { formerType :: Maybe Formertype
  , deleted :: Maybe Deleted
  }
data VideoPart = VideoPart

data Article = Article ObjectPart ArticlePart
data Audio = Audio ObjectPart DocumentPart AudioPart
data Document = Document ObjectPart DocumentPart
data Event = Event ObjectPart EventPart
data Image = Image ObjectPart DocumentPart ImagePart
data Note = Note ObjectPart NotePart
data Page = Page ObjectPart DocumentPart PagePart
data Place = Place ObjectPart PlacePart
data Profile = Profile ObjectPart ProfilePart
data Relationship = Relationship ObjectPart RelationshipPart
data Tombstone = Tombstone ObjectPart TombstonePart
data Video = Video ObjectPart DocumentPart VideoPart

data MentionPart = MentionPart
data Mention = Mention LinkPart MentionPart
-- end Object and Link Types
