#### 1.0.0
* Initial release
  * Specification composition with builder functions and composition expression: `Spec.notEmpty`, `Spec.inclusiveBetween`, `spec { greaterThan }`, ...
  * Access controllers with builder functions and composition express: `Access.onlyUriFromBase`, `Access.once`, `access { revokable }`, ...
  * Access controller types: `ReadOnce` and `Disposable`
  * Untrusted representation with `Untrust<_>`

#### 1.5.0
* Spec additions
  * `startWith(Of)` to verify the header of a `string`
  * `unique(Of)` to verify if a sequence has unique values
  * `createModelWith` to run directly a "cross-world" function (`a -> Result<'b, 'error>`) on the validated value
  * C# basic LINQ function (`Select`, `SelectMany/Then`, `Where`, `Aggregate`, `Zip`, ...) on the `ValidationResult<'T>` type
    together with a `Traverse/Sequence` pair to run directly a series of validation functions on a sequence
 * Type additions
   * `BoundedInt`, an integer that is bound by a min/max threshold
   * `NonEmptySeq<'T>`, a sequence that contains at least one element
   * `UniqueSeq<'T>`, a sequence that only contains unique elements
   * `XmlEncodedString`, a `string` that can be considered a valid XML text (not neccessary a XML element)

#### 1.6.0
* Spec additions
  * `dependsOn` introduces dependent specifications to re-use and split complex specs into more maintainable building blocks
  * `alphabetical`, `alphanum`, ... introduces a way to check a string for charaters only in the alphabeth

#### 2.0.0
* Spec invariants: makes relationships between models more easy to describe
* Maybe: add optional type for better C# domain modeling
* Guard: add simple guard class for quick argument checking, but limit functionality to push users to use specifications instead
* Remove some unnecessary types to limit target projects

#### 3.0.1
* Specifications with tagged errors; using words starting with `@`
* Sanitization blocks for transformations before validation
* `Outcome` type for C# devs
* Extra spec requirements: `containsAll`, `seqEqual`, `structure`...

#### 3.1.0
* Provide `Union` to determine validation based on DU's
* Provide `ErrorMessage` as a means to make more security-safe error messages
* Provide asynchronous `WriteOnce` models
* Provide `String` operations on specifications

#### 3.2.0
* Update project towards .NET 6
* Add missing XML code docs on all specification types
* Fix implicit operator for C# `ValidationResult` with F# `Result` type for easier interop

#### 3.2.1
* Revert wrong renaming of C#-specific `Spec`

#### 3.2.2
* Update `Sanitize` for a more inclusive code base ♥
* Add sanitization composition expression