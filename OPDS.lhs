OPDS Acquisition Selection 1.0
===

This specification describes how the [Library Simplified](https://www.librarysimplified.org)
applications select [OPDS](https://specs.opds.io/opds-1.2) _acquisitions_ for display and
for acquiring books. The specification is described as executable [Literate Haskell](https://www.haskell.org)
and can be executed and inspected directly using [ghci](https://www.haskell.org/ghc/).

Within this document, commands given at the GHCI prompt are prefixed
with `*OPDS>` to indicate that the commands are being executed within
the `OPDS` module.

The main specification definitions are given the [OPDS](OPDS.lhs) module:

```haskell
{-# LANGUAGE Haskell2010, ExplicitForAll #-}

module OPDS where
```

The specification depends on a simple immutable [stack](Stack.hs) definition,
and uses the standard Haskell `Set` data structure:

```haskell
import Stack
import qualified Data.Set as DS
```

URIs and MIME
---

Within this specification, [URI](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier)
and [MIME or Media type](https://en.wikipedia.org/wiki/Media_type) values are
represented as simple strings. Real implementations typically use more involved
data structures (such as [java.net.URI](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/URI.html)).

```haskell
data MIMEType
  = MIMEType String
  deriving (Eq, Ord, Show)

data URI
  = URI String
  deriving (Eq, Ord, Show)
```

OPDS link relations are given as a simple enumerated type:

```haskell
data Relation
  = Borrow
  | Buy
  | Generic
  | OpenAccess
  | Sample
  | Subscribe
  deriving (Eq, Show)
```

Acquisitions
---

An OPDS _acquisition_ is an XML `link` element with zero or more _indirect acquisition_
elements. An _acquisition_ *MUST* specify a starting URI and MIME type, and each
_indirect acquisition_ *MUST* specify MIME type. An acquisition `a` holding
a URI `u` of MIME type `t` indicates that a client application may fetch an
object of type `t` by making a request at `u`. _Indirect acquisitions_ form
a directed acyclic graph with an _acquisition_ at the root.
_Indirect_ acquisitions do not carry URI values and therefore rely on client
applications using some form of out-of-band mechanism to fetch the next
object in the graph when traversing it. As a concrete example, a client
may fetch an Adobe ACSM file directly from a URI via an _acquisition_, and this
ACSM file can then be used by a third-party module to fetch data of a type
specified in a subsequent _indirect_ acquisition.

```haskell
data IndirectAcquisition
  = IndirectAcquisition MIMEType [IndirectAcquisition]
  deriving (Eq, Show)

data Acquisition
  = Acquisition Relation URI MIMEType [IndirectAcquisition]
  deriving (Eq, Show)
```

An _acquisition path_ is a list of acquisitions (indirect or otherwise) through
which an application must traverse in order to obtain the desired target
object. For example, an application may traverse through an OPDS acquisition
feed entry, followed by an Adobe ACSM file, finally obtaining an encrypted
EPUB file.

```haskell
data AcquisitionPathElement
  = AcquisitionPathElement MIMEType (Maybe URI)
  deriving (Eq, Show)

data AcquisitionPath
  = AcquisitionPath [AcquisitionPathElement]
  deriving (Eq, Show)
```

This specification defines a couple of convenience functions for prefixing
elements to _acquisition paths_, and for extracting the MIME types of all
of the elements of the path in declaration order:

```haskell
prefixPathWithElement :: AcquisitionPathElement -> AcquisitionPath -> AcquisitionPath
prefixPathWithElement e (AcquisitionPath xs) = AcquisitionPath $ [e] ++ xs

prefixPathWith :: MIMEType -> (Maybe URI) -> AcquisitionPath -> AcquisitionPath
prefixPathWith mime uri = prefixPathWithElement (AcquisitionPathElement mime uri)

mimeTypesOf :: AcquisitionPath -> [MIMEType]
mimeTypesOf (AcquisitionPath e) = map (\(AcquisitionPathElement mime _) -> mime) e
```

An OPDS _acquisition feed entry_ contains zero or more _acquisitions_ along
with an identifier that uniquely identifies the entry. The identifier is not
important to this specification, but is represented here as a string value
in order to more easily identify the example entries given later in this
specification.

```haskell
data FeedEntry
  = FeedEntry String [Acquisition]
  deriving (Eq, Show)
```

Example Acquisitions
---

The following acquisition provides a single, directly-accessible EPUB file:

```xml
<link href="https://example.com/Open-Access"
      type="application/epub+zip"
      rel="http://opds-spec.org/acquisition/open-access"/>
```

The following acquisition provides an OPDS acquisition feed that will lead
to an Adobe ACSM file, that can lead to either an encrypted PDF file or an
encrypted EPUB file:

```xml
<link href="http://example.com/borrow" rel="http://opds-spec.org/acquisition" type="application/atom+xml;relation=entry;profile=opds-catalog">
  <opds:indirectAcquisition type="application/vnd.adobe.adept+xml">
    <opds:indirectAcquisition type="application/epub+zip"/>
    <opds:indirectAcquisition type="application/pdf"/>
  </opds:indirectAcquisition>
</link>
```

Linearized Acquisitions
---

Applications should _linearize_ the tree of acquisitions, preserving the
order of acquisitions as declared in the originating OPDS feed. This is
achieved by performing a depth-first traversal over the tree of acquisitions,
maintaining a stack of the elements that lead to the current acquisition,
and producing a list of elements at each leaf node.

```haskell
indirectPathsInner :: Stack AcquisitionPathElement -> IndirectAcquisition -> [AcquisitionPath]
indirectPathsInner st (IndirectAcquisition mime [])  =
  [AcquisitionPath $ (stackList st) ++ [AcquisitionPathElement mime Nothing]]
indirectPathsInner st (IndirectAcquisition mime ixs) =
  let st' = stackPush st (AcquisitionPathElement mime Nothing) in
    concatMap (indirectPathsInner st') ixs

indirectPaths :: IndirectAcquisition -> [AcquisitionPath]
indirectPaths = indirectPathsInner (Stack [])

acquisitionPaths :: Acquisition -> [AcquisitionPath]
acquisitionPaths (Acquisition _ uri mime []) =
  [AcquisitionPath [AcquisitionPathElement mime $ Just uri]]
acquisitionPaths (Acquisition _ uri mime indirects) =
  let iPaths = concatMap indirectPaths indirects in
    map (prefixPathWith mime (Just uri)) iPaths

acquisitionPathsAll :: [Acquisition] -> [AcquisitionPath]
acquisitionPathsAll = concatMap acquisitionPaths

acquisitionPathsFeedEntry :: FeedEntry -> [AcquisitionPath]
acquisitionPathsFeedEntry (FeedEntry _ xs) = acquisitionPathsAll xs
```

Linearization Examples
---

This section makes use of a `ShowablePretty` class used to more concisely
display acquisition paths.

```haskell
class ShowablePretty a where
  showPretty :: a -> String

instance ShowablePretty AcquisitionPathElement where
  showPretty (AcquisitionPathElement (MIMEType mime) Nothing)          = mime
  showPretty (AcquisitionPathElement (MIMEType mime) (Just (URI uri))) = "(" ++ mime ++ "," ++ uri ++ ")"

instance ShowablePretty AcquisitionPath where
  showPretty (AcquisitionPath [])       = ""
  showPretty (AcquisitionPath (x : xs)) =
    if length xs > 0
    then (showPretty x) ++ " -> " ++ (showPretty $ AcquisitionPath xs)
    else showPretty x
```

The following is a list of example URIs and MIME types used in the examples:

```haskell
uriOpen :: URI
uriOpen = URI "https://example.com/Open-Access"
uriBorrow :: URI
uriBorrow = URI "https://example.com/Borrow"
uriFulfill :: URI
uriFulfill = URI "https://example.com/Fulfill"

mimePDF :: MIMEType
mimePDF = MIMEType "application/pdf"
mimeEPUB :: MIMEType
mimeEPUB = MIMEType "application/epub+zip"
mimePlain :: MIMEType
mimePlain = MIMEType "text/plain"
mimeACSM :: MIMEType
mimeACSM = MIMEType "application/vnd.adobe.adept+xml"
mimeOPDS :: MIMEType
mimeOPDS = MIMEType "application/atom+xml;relation=entry;profile=opds-catalog"
mimeHTML :: MIMEType
mimeHTML = MIMEType "text/html"
```

A trivial example that shows a single open access EPUB accessible directly
without the use of any indirect acquisitions:

```haskell
exampleAcqOpen0 :: Acquisition
exampleAcqOpen0 = Acquisition OpenAccess uriOpen mimeEPUB []
exampleFeedOpenAccess0 :: FeedEntry
exampleFeedOpenAccess0 =
  FeedEntry "dae12801-6b76-4a36-825d-a385c045e0b4" [exampleAcqOpen0]
```

The linearization of this feed entry shows a single acquisition path:

```
*OPDS> map showPretty $ acquisitionPathsFeedEntry exampleFeedOpenAccess0
["(application/epub+zip,https://example.com/Open-Access)"]
```

A more complex example that shows two possible acquisition paths, both of
which require obtaining an Adobe ACSM file and will yield either an encrypted
PDF file or an encrypted EPUB file. Note that the EPUB file is specified earlier
in the entry, and so appears earlier in the output paths.

```haskell
exampleAdobePDF0 :: Acquisition
exampleAdobePDF0 =
  Acquisition Generic uriFulfill mimeACSM [IndirectAcquisition mimePDF []]
exampleAdobeEPUB0 :: Acquisition
exampleAdobeEPUB0 =
  Acquisition Generic uriFulfill mimeACSM [IndirectAcquisition mimeEPUB []]
exampleFeedAdobeIndirect0 :: FeedEntry
exampleFeedAdobeIndirect0 =
  FeedEntry "d5e47d8e-4569-424c-900c-e2720f10f7d0" [exampleAdobeEPUB0, exampleAdobePDF0]
```

```
*OPDS> map showPretty $ acquisitionPathsFeedEntry exampleFeedAdobeIndirect0
["(application/vnd.adobe.adept+xml,https://example.com/Fulfill) -> application/epub+zip",
 "(application/vnd.adobe.adept+xml,https://example.com/Fulfill) -> application/pdf"]
```

Another complex example that shows four possible acquisition paths. Three
of the paths require first going through an OPDS acquisition feed entry,
followed by an Adobe ACSM file, and will yield either a PDF, EPUB, or plain
text file. The other path fetches an HTML file directly.

```haskell
exampleHTML0 :: Acquisition
exampleHTML0 = Acquisition OpenAccess uriOpen mimeHTML []

exampleMulti0 :: Acquisition
exampleMulti0 =
  Acquisition Borrow uriBorrow mimeOPDS [
    IndirectAcquisition mimeACSM [IndirectAcquisition mimePDF []],
    IndirectAcquisition mimeACSM [IndirectAcquisition mimeEPUB []],
    IndirectAcquisition mimeACSM [IndirectAcquisition mimePlain []]
  ]

exampleFeedMulti0 :: FeedEntry
exampleFeedMulti0 =
  FeedEntry "c736c012-2c93-49e5-94ed-9acfa1a0f846" [exampleMulti0, exampleHTML0]
```

```
*OPDS> map showPretty $ acquisitionPathsFeedEntry exampleFeedMulti0
["(application/atom+xml;relation=entry;profile=opds-catalog,https://example.com/Borrow) -> application/vnd.adobe.adept+xml -> application/pdf",
 "(application/atom+xml;relation=entry;profile=opds-catalog,https://example.com/Borrow) -> application/vnd.adobe.adept+xml -> application/epub+zip",
 "(application/atom+xml;relation=entry;profile=opds-catalog,https://example.com/Borrow) -> application/vnd.adobe.adept+xml -> text/plain",
 "(text/html,https://example.com/Open-Access)"]
```

Acquisition Path Filtering
---

Applications are often unable to support one or more data types delivered
by OPDS acquisition feed entries. An application _may_ support a given
acquisition path `P` if the application supports each and every MIME type
given by `mimeTypesOf P`.

```haskell
supportsEach :: DS.Set MIMEType -> AcquisitionPath -> Bool
supportsEach supported path = all (\t -> DS.member t supported) (mimeTypesOf path)
```

Applications _MUST_ filter unsupported acquisition paths whilst preserving
the declaration order of those paths as they appeared in the original OPDS
feed entry. Applications _MAY_ provide extra filtering of acquisition paths
beyond that specified here as long as the declaration order of paths remains
preserved.

```haskell
supportedPaths :: DS.Set MIMEType -> FeedEntry -> [AcquisitionPath]
supportedPaths supported entry = filter (supportsEach supported) (acquisitionPathsFeedEntry entry)
```

The preservation of the declaration order of acquisition paths in feeds
allows for OPDS servers to effectively set the preferred acquisitions for
clients; acquisitions that the server hopes clients will use the most should
be declared first in the feed. Applications _SHOULD_ use the first element of
the list of filtered acquisition paths as the default choice for obtaining an
object from an OPDS feed.

Acquisition Path Filtering Example
---

An application with no supported MIME types reports no acquisition paths:

```
*OPDS> supportedPaths DS.empty exampleFeedMulti0
[]
```

An application that does not support Adobe ACSM files reports only one
acquisition path:

```
*OPDS> map showPretty $ supportedPaths (DS.fromList [mimeOPDS, mimePDF, mimeEPUB, mimePlain, mimeHTML]) exampleFeedMulti0
["(text/html,https://example.com/Open-Access)"]
```
