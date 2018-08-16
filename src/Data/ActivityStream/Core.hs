{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Data.ActivityStream.Core where

import Import

data ObjectPart = ObjectPart
  { attachment :: Maybe Attachment
  , attributedTo :: Maybe Attributedto
  , audience :: Maybe Audience
  , content :: Maybe ContentProp
  , context :: Maybe Context
  , name :: Maybe Name
  , endTime :: Maybe Endtime
  , generator :: Maybe Generator
  , icon :: Maybe Icon
  , image :: Maybe Image
  , inReplyTo :: Maybe Inreplyto
  , location :: Maybe Location
  , preview :: Maybe Preview
  , published :: Maybe Published
  , replies :: Maybe Replies
  , startTime :: Maybe Starttime
  , summary :: Maybe Summary
  , tag :: Maybe Tag
  , updated :: Maybe Updated
  , url :: Maybe Url
  , to :: Maybe To
  , bto :: Maybe Bto
  , cc :: Maybe Cc
  , bcc :: Maybe Bcc
  , mediaType :: Maybe Mediatype
  , duration :: Maybe Duration
  , id :: Maybe Id
  , oType :: Maybe Type
  } deriving (Show)

data LinkPart = LinkPart
  { href :: Maybe Href
  , rel :: Maybe Rel
  , mediaType :: Maybe Mediatype
  , name :: Maybe Name
  , hreflang :: Maybe Hreflang
  , height :: Maybe Height
  , width :: Maybe Width
  , preview :: Maybe Preview
  , id :: Maybe Id
  , lType :: Maybe Type
  , attributedTo :: Maybe Attributedto
  } deriving (Show)

data ActivityPart = ActivityPart
  { actor :: Maybe Actor
  , object :: Maybe ObjectProp
  , target :: Maybe Target
  , result :: Maybe Result
  , origin :: Maybe Origin
  , instrument :: Maybe Instrument
  } deriving (Show)

data IntransitiveActivityPart = IntransitiveActivityPart
  deriving (Show)

data CollectionPart = CollectionPart
  { totalItems :: Maybe Totalitems
  , current :: Maybe Current
  , first :: Maybe First
  , last :: Maybe Last
  , items :: Maybe Items
  } deriving (Show)

data OrderedCollectionPart = OrderedCollectionPart
  deriving (Show)

data CollectionPagePart = CollectionPagePart
  { partOf :: Maybe Partof
  , next :: Maybe Next
  , prev :: Maybe Prev
  } deriving (Show)

data OrderedCollectionPagePart = OrderedCollectionPagePart
  { startIndex :: Maybe Startindex
  } deriving (Show)

data Object = Object ObjectPart deriving (Show)

data Link = Link LinkPart deriving (Show)

data Activity = Activity ObjectPart ActivityPart deriving (Show)

data IntransitiveActivity = IntransitiveActivity ObjectPart ActivityPart IntransitiveActivityPart deriving (Show)

data Collection = Collection ObjectPart CollectionPart deriving (Show)

data OrderedCollection = OrderedCollection ObjectPart CollectionPart OrderedCollectionPart deriving (Show)

data CollectionPage = CollectionPage ObjectPart CollectionPart CollectionPagePart deriving (Show)

data OrderedCollectionPage = OrderedCollectionPage ObjectPart CollectionPart OrderedCollectionPart CollectionPagePart deriving (Show)


-- Properties

type URI = Text
data ValueOrList a = Value a | List [a] deriving (Show)

type ObjectOrLink = Either Object Link
type CollectionPageOrLink = Either CollectionPage Link
data ImageOrLink = IL (Either Image Link) deriving (Show)

type XsdDatetime = Text
type XsdBoolean = Text
type XsdDuration = Text
type XsdNonNegativeInteger = Int

data ClosedT = ClosedO Object | ClosedL Link | ClosedD XsdDatetime | ClosedB XsdBoolean deriving (Show)

type UriOrLink = Either URI Link
type CollectionOrLink = Either Collection Link

type LanguageTag = Text
type MIMEMediaType = Text

type Actor = ValueOrList ObjectOrLink
type Attachment = ValueOrList ObjectOrLink
type Attributedto = ValueOrList ObjectOrLink
type Audience = ValueOrList ObjectOrLink
type Bcc = ValueOrList ObjectOrLink
type Bto = ValueOrList ObjectOrLink
type Cc = ValueOrList ObjectOrLink
type Context = ValueOrList ObjectOrLink
type Current = CollectionPageOrLink
type First = CollectionPageOrLink
type Generator = ValueOrList ObjectOrLink
type Icon = ValueOrList ImageOrLink
type Id = URI
type Image = ValueOrList ImageOrLink
type Inreplyto = ValueOrList ObjectOrLink
type Instrument = ValueOrList ObjectOrLink
type Last = CollectionPageOrLink
type Location = ValueOrList ObjectOrLink
data Items = Items deriving (Show)
type Oneof = ValueOrList ObjectOrLink
type Anyof = ValueOrList ObjectOrLink
data Closed = ValueOrList ClosedT deriving (Show)
type Origin = ValueOrList ObjectOrLink
type Next = CollectionPageOrLink
type ObjectProp = ValueOrList ObjectOrLink
type Prev = CollectionPageOrLink
type Preview = ValueOrList ObjectOrLink
type Result = ValueOrList ObjectOrLink
type Replies = Collection
type Tag = ValueOrList ObjectOrLink
type Target = ValueOrList ObjectOrLink
type To = ValueOrList ObjectOrLink
type Type = ValueOrList URI
type Url = ValueOrList UriOrLink
type Accuracy = Float
type Altitude = Float
type ContentProp = ValueOrList Text
type Name = ValueOrList Text
type Duration = XsdDuration
type Height = XsdNonNegativeInteger
type Href = URI
type Hreflang = LanguageTag
type Partof = CollectionOrLink
type Latitude = Float
type Longitude = Float
type Mediatype = MIMEMediaType
type Endtime = XsdDatetime
type Published = XsdDatetime
type Starttime = XsdDatetime
type Radius = Float
type Rel = ValueOrList Text
type Startindex = XsdNonNegativeInteger
type Summary = ValueOrList Text
type Totalitems = XsdNonNegativeInteger
data Units = Cm | Feet | Inches | Km | M | Miles | UUri URI deriving (Show)
type Updated = XsdDatetime
type Width = XsdNonNegativeInteger
type Subject = ObjectOrLink
type Relationship = Object
type Describes = Object
type Formertype = Object
type Deleted = XsdDatetime

