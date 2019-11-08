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