using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.IO;
using System.Net.Mail;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Schema;
using FPrimitive;
using Microsoft.FSharp.Core;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Schema;

namespace FPrimitive.CSharp.Tests
{
    public class Examples
    {
        public void Test1<T>()
        {
            // Specification before the actual check of the base-uri.
            Spec<Uri> spec =
                Spec.Of<Uri>("uri")
                    .NotNull("uri should not be 'null'")
                    .Equal(uri => uri.Scheme, Uri.UriSchemeHttps, "@scheme should be 'https' scheme");

            ValidationResult<Uri> validationResult = spec.Validate(new Uri("http://localhost", UriKind.Relative));

            // Demonstration purposes, generic type could be anything.
            IObservable<T> obs = null;

            // Access controlled function with validation, revocation, limited amount of evaluations and time-based availability.
            Access<Uri, Uri> access =
                Access.OnlyUriFromBase(new Uri("https://localhost:9090/"))
                      .Satisfy(spec)
                      .Once()
                      .DuringHours(9, 17)
                      .RevokedWhen(obs);

            // Somewhere else...
            AccessResult<Uri> result = access.Eval(new Uri("http://localhost:9090/path"));
            if (result.TryGetValue(out Uri output))
            {
                // use the now correct 'output'
            }

            int unknown = 5;
            Untrust<int> untrusted = unknown;

            ValidationResult<NonZeroInt> nonZeroIntValidationResult = 
                Spec.Of<int>()
                    .GreaterThan(0, "should not be zero")
                    .CreateModel(untrusted, i => new NonZeroInt(i));

            Outcome<EbmsString, IDictionary<string, string[]>> ebmsStringResult = EbmsString.Create("mycustomid");

            var int100 = Valid<int>.Create(9, x => x > 0 && x < 100, "should be between 1-100");

            Outcome<EbmsString, Exception> deserialize = Json.Deserialize<EbmsString>(new Valid<JObject>());
            deserialize.Do(ex => throw ex);

            var nonEmptyResult = NonEmptyEnumerable<string>.Create(new[] { "test1", "test2", "test3" });
            var uniqueResult = UniqueEnumerable<int>.Create(new[] { 1, 2, 3 });
        }

        private async Task<Outcome<string, string[]>> GetRawSecretAsync()
        {
            AccessResult<string> result =
                await Access.Function<string, Task<string>>(secretName => Task.FromResult("secretValue"))
                            .EvalAsync("secretName");
            return result;
        }

        private async Task<Outcome<string, int>> GetSecretAsync()
        {
            return await Access.Function<string, Task<Outcome<string, int>>>(secretName => Task.FromResult(System.Outcome.Success<string, int>("secret")))
                               .EvalOutcomeAsync("secretName", errors => 1);
        }
    }

    public class EbmsString
    {
        private readonly string _value;

        private EbmsString(string value)
        {
            _value = value;
        }

        public static ValidationResult<EbmsString> Create(string value)
        {
            var pattern = "^[a-z-A-Z0-9]$";
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("ebMS string cannot be null or blank")
                       .Matches(pattern, $"ebMS string can only contain chars that matches {pattern}")
                       .CreateModel(value, validated => new EbmsString(validated));
        }
    }

    public class Agreement
    {
        private Agreement(EbmsString value, Maybe<EbmsString> type, Maybe<EbmsString> pmode)
        {
            Value = value;
            Type = type;
            PMode = pmode;
        }

        public EbmsString Value { get; }
        public Maybe<EbmsString> Type { get; }
        public Maybe<EbmsString> PMode { get; }

        public static ValidationResult<Agreement> Create(string value, string type, string pmode)
        {
            var valueResult = EbmsString.Create(value);
            var typeResult = EbmsString.Create(type).ToMaybe();
            var pmodeResult = EbmsString.Create(pmode).ToMaybe();

            return valueResult.Select(v => new Agreement(v, typeResult, pmodeResult));
        }
    }

    public class NonZeroInt
    {
        private readonly int _value;

        public NonZeroInt(int value)
        {
            if (value == 0)
            {
                throw new ArgumentException("Value should not be zero");
            }

            _value = value;
        }
    }

    public class ReadableText
    {
        private readonly string _value;
        private ReadableText(string name)
        {
            _value = name;
        }

        public static ValidationResult<ReadableText> Create(string untrusted)
        {
            return Spec.Of<string>("name")
                       .NotNull("Product @name should not be 'null'")
                       .NotEmpty("Product @name should not be empty")
                       .NotWhiteSpace("Product @name should not contain only white-space characters")
                       .Matches("^[a-zA-Z ]+$", "Product @name should only contain characters from a-z")
                       .Add(input => input.Length == 20, "Product @name should only have a length of 20")
                       .Cascade(CascadeMode.FirstFailure)
                       .CreateModel(untrusted, validated => new ReadableText(validated));
        }
    }

    public class Int1To20
    {
        private readonly int _value;
        private Int1To20(int value)
        {
            _value = value;
        }

        public static ValidationResult<Int1To20> Create(int untrusted)
        {
            return Spec.Of<int>()
                       .InclusiveBetween(1, 20, "Integer should be between 1-20")
                       .Cascade(CascadeMode.Continue)
                       .CreateModel(untrusted, validated => new Int1To20(validated));
        }
    }

    public class Record
    {
        private Record(ReadableText name, Int1To20 amount)
        {
            ProductName = name;
            Amount = amount;
        }

        public ReadableText ProductName { get; }
        public Int1To20 Amount { get; }

        public static ValidationResult<Record> Create(string productName, int amount)
        {
            return ReadableText.Create(productName)
                .Zip(Int1To20.Create(amount),
                     (txt, integer) => new Record(txt, integer));
        }
    }

    public class Name
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="Name"/> class.
        /// </summary>
        private Name(string value)
        {
            Value = value;
        }

        public string Value { get; }

        // public ValidationResult<Name> Create(string value)
        // {
        //     return Spec.Of<string>()
        //         .NotNull("Name should not be 'null'")
        //         .NotEmpty("Name should not be empty")
        //         .CreateModel(value, x => new Name(x));
        // }

        // public static Name CreateOrThrow(string value)
        // {
        //     return Spec.Of<string>()
        //         .NotNull("Name should not be 'null'")
        //         .NotEmpty("Name should not be empty")
        //         .Cascade(CascadeMode.Continue)
        //         .CreateModelOrThrow(value, x => new Name(x), "Cannot create a Name instance");
        // }

        // public static bool TryCreate(string value, out Name output)
        // {
        //     return Spec.Of<string>()
        //         .NotNull("Name should not be 'null'")
        //         .NotEmpty("Name should not be empty")
        //         .TryCreateModel(value, x => new Name(x), out output);
        // }
    }

    public class Point
    {
        private Point(int min, int max)
        {
            var minSpec =
                Spec.Of<int>()
                    .GreaterThan(0, "minimum should be greater than zero");

            var maxSpec =
                Spec.Of<int>()
                    .LessThan(100, "maximum should be less than 100");

            Spec.Invariant(minSpec, maxSpec)
                .Add(t => t.Item1 < t.Item2, "minimum should be less than maximum")
                .ValidateThrow<(int, int), ValidationFailureException>((min, max), "Validation failure");

            Min = min;
            Max = max;
        }

        public int Min { get; }
        public int Max { get; }

        public static Point Create(int min, int max)
        {
            return Spec.Of<(int min, int max)>()
                .InclusiveBetween(x => x.min, 0, 10, "Minimum should be between 1-10")
                .InclusiveBetween(x => x.max, 11, 20, "Maximum should be between 11-20")
                .CreateModelOrThrow((min: min, max: max), x => new Point(x.min, x.max), "Could not create Point instance");
        }
    }

    public class NonWhiteSpaceString
    {
        private readonly string _value;

        public NonWhiteSpaceString(string value)
        {
            if (String.IsNullOrWhiteSpace(value))
            {
                throw new ArgumentException("Value cannot be null or blank", nameof(value));
            }

            _value = value;
        }

        public static ValidationResult<NonWhiteSpaceString> Create(string value)
        {
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("value cannot be null or blank")
                       .CreateModel(value, x => new NonWhiteSpaceString(x));
        }
    }

    public class Party
    {
        public Party(UniqueEnumerable<NonWhiteSpaceString> ids)
        {
            Ids = ids;
        }

        public UniqueEnumerable<NonWhiteSpaceString> Ids { get; }

        public static ValidationResult<Party> Create(IEnumerable<string> ids)
        {
            return NonEmptyEnumerable<string>.Create(ids)
                .Then(xs => xs.Traverse(NonWhiteSpaceString.Create))
                .Then(xs => xs.AsUnique())
                .Select(xs => new Party(xs));
        }
    }

    public class ZipCode : IEquatable<ZipCode>
    {
        private readonly int _number;

        private ZipCode(int number)
        {
            _number = number;
        }

        public static ValidationResult<ZipCode> Create(int number)
        {
            return Spec.Of<int>()
                       .InclusiveBetween(0, 9999, "should be greater than or equal to zero but less than or equal to 9999")
                       .CreateModel(number, n => new ZipCode(n));
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return _number.ToString("D4");
        }

        /// <summary>Indicates whether the current object is equal to another object of the same type.</summary>
        /// <param name="other">An object to compare with this object.</param>
        /// <returns>true if the current object is equal to the <paramref name="other">other</paramref> parameter; otherwise, false.</returns>
        public bool Equals(ZipCode other)
        {
            if (other is null) return false;
            if (ReferenceEquals(this, other)) return true;

            return _number == other._number;
        }

        /// <summary>Determines whether the specified object is equal to the current object.</summary>
        /// <param name="obj">The object to compare with the current object.</param>
        /// <returns>true if the specified object  is equal to the current object; otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            if (obj is null) return false;
            if (ReferenceEquals(this, obj)) return true;

            return obj is ZipCode code && Equals(code);
        }

        /// <summary>Serves as the default hash function.</summary>
        /// <returns>A hash code for the current object.</returns>
        public override int GetHashCode()
        {
            return _number.GetHashCode();
        }

        /// <summary>Returns a value that indicates whether the values of two <see cref="T:FPrimitive.CSharp.Tests.ZipCode" /> objects are equal.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if the <paramref name="left" /> and <paramref name="right" /> parameters have the same value; otherwise, false.</returns>
        public static bool operator ==(ZipCode left, ZipCode right)
        {
            return Equals(left, right);
        }

        /// <summary>Returns a value that indicates whether two <see cref="T:FPrimitive.CSharp.Tests.ZipCode" /> objects have different values.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator !=(ZipCode left, ZipCode right)
        {
            return !Equals(left, right);
        }
    }

    public class Password
    {
        private readonly ReadOnce<string> _password;

        /// <summary>
        /// Initializes a new instance of the <see cref="Password"/> class.
        /// </summary>
        public Password(string password)
        {
            _password = new ReadOnce<string>(password);
        }

        public string GetValue()
        {
            return _password.GetValue();
        }
    }

    public class Username
    {
        private readonly string _value;

        private Username(string value)
        {
            _value = value;
        }

        public static ValidationResult<Username> Create(string value)
        {
            var pattern = @"^[a-z0-9_-]{3,16}$";
            return Spec.Of<string>()
                       .Matches(pattern, $"should match username pattern: {pattern}")
                       .CreateModel(value, validated => new Username(validated));

        }
    }

    public class Url
    {
        private readonly string _value;

        private Url(string value)
        {
            _value = value;
        }

        public static ValidationResult<Url> Create(string value)
        {
            var regex = @"^(https?://)+\.([/\w \.-]+)+/$";

            return Spec.Of<string>()
                       .Matches(regex, $"URL should match pattern: {regex}")
                       .CreateModel(value, validated => new Url(validated));
        }
    }

    public class Author
    {
        private readonly string _name;

        private Author(string name)
        {
            _name = name;
        }

        public static ValidationResult<Author> Create(string author)
        {
            const string name = @"[A-Z]{1}[a-z]*\.?";
            string pattern = $@"^({name})( {name})*$";
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("author name should not be blank")
                       .Add(s => s.Length > 1, "author name should at least be a single character")
                       .Matches(pattern, $"author name should match regular expression: {pattern}")
                       .CreateModel(author, validated => new Author(validated));
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return _name;
        }
    }

    public class ISBN13
    {
        private readonly string _code;

        private ISBN13(string code)
        {
            _code = code;
        }

        public static ValidationResult<ISBN13> Create(string code)
        {
            const string pattern = "^[0-9]+$";
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("ISBN13 number should not be blank")
                       .Add(s => s.Length == 13, "ISBN13 number should be 13 characters long")
                       .Matches(pattern, $"ISBN13 number should match regular expression: {pattern}")
                       .StartsWith("978", "ISBN13 number should start with '978'")
                       .Add(Checksum, "ISBN13 checksum was invalid")
                       .CreateModel(code, validated => new ISBN13(validated));
        }

        private static bool Checksum(string code)
        {
            IEnumerable<int> numbers = 
                code.ToCharArray()
                    .Select(c => Int32.Parse(c.ToString()));

            int sumNumbers = 
                numbers.Take(12)
                       .Select((n, i) => i % 2 != 0 ? n * 3 : n)
                       .Sum();

            int remaining = sumNumbers % 10;
            int checksum = remaining != 0 ? 10 - remaining : remaining;

            int lastNumber = numbers.Last();
            return checksum == lastNumber;
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return String.Format("{0:###-#-###-####-#}", _code);
        }
    }

    public class Int1000
    {
        private readonly int _value;

        private Int1000(int value)
        {
            _value = value;
        }

        public static ValidationResult<Int1000> Create(int value)
        {
            return Spec.Of<int>()
                       .InclusiveBetween(0, 1000, "integer number must be between 1-1000")
                       .CreateModel(value, validated => new Int1000(validated));
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return _value.ToString();
        }
    }

    public enum RatingEnum
    {
        Unforgettable,
        VeryGood,
        Okay,
        AlmostRight,
        Garbage
    }

    public static class Rating
    {
        public static ValidationResult<RatingEnum> Create(string value)
        {
            ValidationResult<RatingEnum> validationResult = 
                Spec.Of<string>()
                    .NotNullOrWhiteSpace("Rating cannot be blank")
                    .CreateModel(value, untrusted => 
                        Enum.TryParse(untrusted, out RatingEnum result) 
                            ? ValidationResult<RatingEnum>.Success(result)
                            : ValidationResult<RatingEnum>.Failure("Cannot convert value to rating type"));

            return validationResult;
        }
    }

    public class Book
    {
        private readonly Author _author;
        private readonly ISBN13 _isbn13;
        private readonly Int1000 _pages;
        private readonly Maybe<RatingEnum> _rating;

        private Book(Author author, ISBN13 isbn13, Int1000 pages, Maybe<RatingEnum> rating)
        {
            _author = author;
            _isbn13 = isbn13;
            _pages = pages;
            _rating = rating;
        }

        public static ValidationResult<Book> Create(string author, string isbn13, int pages, string rating)
        {
            ValidationResult<Author> authorResult = Author.Create(author);
            ValidationResult<ISBN13> isbn13Result = ISBN13.Create(isbn13);
            ValidationResult<Int1000> pagesResult = Int1000.Create(pages);
            ValidationResult<Maybe<RatingEnum>> ratingResult = Spec.Optional(x => !String.IsNullOrWhiteSpace(x), Rating.Create, rating);

            return ValidationResult.Combine(authorResult, isbn13Result, pagesResult, ratingResult, (auth, isbn, ps, r) => new Book(auth, isbn, ps, r));
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return $"Book by {_author} with {_pages} pages, ISBN: {_isbn13}";
        }
    }

    public static class Map
    {
        public static Outcome<Book, IDictionary<string, string[]>> ToModel(Maybe<BookJson> jsonM)
        {
            if (jsonM.TryGetValue(out BookJson json))
            {
                return Book.Create(json.Author, json.ISBN13, json.Pages, null);
            }

            return ValidationResult.Error<Book>("book", "Cannot create book domain model of unknown JSON book representation");
        }
    }

    public class BookJson
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="BookJson"/> class.
        /// </summary>
        [JsonConstructor]
        public BookJson(string author, string isbn13, int pages)
        {
            Author = Sanitize.RegexReplace("( ){2,}", String.Empty, author).WhiteMatch(@"[a-zA-Z\. ]+");
            ISBN13 = Sanitize.RemoveWhitespace(isbn13).BlackList(@"\-");
            Pages = pages;
        }

        [JsonProperty("author"), System.ComponentModel.DefaultValue("")]
        public string Author { get; }

        [JsonProperty("isbn13"), System.ComponentModel.DefaultValue("")]
        public string ISBN13 { get; }

        [JsonProperty("pages")]
        public int Pages { get; }

        [JsonIgnore]
        public static JSchema Schema
        {
            get
            {
                var schema = JSchema.Parse(
                    @"{ 'description': 'A book',
                        'type': 'object',
                        'properties': 
                          { 'author': { 'type': 'string', 'minLength': 1, 'maxLength': 100 },
                            'isbn13': { 'type': 'string', 'minLength': 13, 'maxLength': 17 },
                            'pages': { 'type': 'number', 'minimum': 1, 'maximum': 1000 } } }");

                schema.AllowAdditionalProperties = false;

                return schema;
            }
        }
    }

    public static class Json
    {
        public static Outcome<JObject, string[]> Load(string content)
        {
            if (content is null)
            {
                throw new JsonException("Blank content cannot be parsed to valid JSON");
            }

            var settings = new JsonLoadSettings
            {
                CommentHandling = CommentHandling.Ignore,
                LineInfoHandling = LineInfoHandling.Ignore
            };

            using (var strReader = new StringReader(content))
            using (var reader = new JsonTextReader(strReader))
            {
                try
                {
                    return JObject.Load(reader, settings);
                }
                catch (JsonReaderException)
                {
                    return new[] { "Content cannot be parsed to valid JSON" };
                }
            }
        }

        public static async Task<OutcomeC<JObject, string[]>> LoadAsync(string content)
        {
            if (content is null)
            {
                throw new JsonException("Blank content cannot be parsed to valid JSON");
            }

            var settings = new JsonLoadSettings
            {
                CommentHandling = CommentHandling.Ignore,
                LineInfoHandling = LineInfoHandling.Ignore
            };

            using (var strReader = new StringReader(content))
            using (var reader = new JsonTextReader(strReader))
            {
                try
                {
                    return await JObject.LoadAsync(reader, settings);
                }
                catch (JsonReaderException)
                {
                    return new[] { "Content cannot be parsed to valid JSON" };
                }
            }
        }

        public static Outcome<Valid<JObject>, string[]> Validate(JObject obj, JSchema sch)
        {
            bool isValid = obj.IsValid(sch, out IList<string> errorMessages);
            return Valid<JObject>.Create(obj, _ => isValid, errorMessages.ToArray());
        }

        public static Outcome<T, Exception> Deserialize<T>(Valid<JObject> obj, int maxDepth = 1)
        {
            var serializer = new JsonSerializer
            {
                NullValueHandling = NullValueHandling.Ignore,
                TypeNameHandling = TypeNameHandling.None,
                MaxDepth = maxDepth,
            };

            using (var jtokenReader = new JTokenReader(obj))
            {
                try
                {
                    return serializer.Deserialize<T>(jtokenReader);
                }
                catch (Exception ex)
                {
                    return ex;
                }
            }
        }
    }

    public class MessagePropertyXml
    {
        public string Name { get; set; }

        public string Value { get; set; }
    }

    public class UserMessageXml
    {
        public int[] Transactions { get; set; }
        public MessagePropertyXml[] Properties { get; set; }
    }

    public class MessageProperty
    {
        private MessageProperty(string name, string value)
        {
            Name = name;
            Value = value;
        }

        public string Name { get; }
        public string Value { get; }

        public static ValidationResult<MessageProperty> Create(MessagePropertyXml xml)
        {
            return Spec.Of<MessagePropertyXml>()
                       .NotNull("cannot create message property from 'null' input")
                       .NotNullOrWhiteSpace(x => x.Name, "cannot create message property with blank name")
                       .NotNullOrWhiteSpace(x => x.Value, "cannot create message property with blank value")
                       .CreateModel(xml, validated => new MessageProperty(validated.Name, validated.Value));
        }
    }

    public class UserMessage
    {
        private readonly NonEmptyEnumerable<int> _transactions;
        private readonly UniqueEnumerable<MessageProperty, string> _properties;

        private UserMessage(
            NonEmptyEnumerable<int> transactions,
            UniqueEnumerable<MessageProperty, string> properties)
        {
            _transactions = transactions;
            _properties = properties;
        }

        public static ValidationResult<UserMessage> Create(UserMessageXml userMessage)
        {
            return Spec.Of<UserMessageXml>()
                       .NotNull("cannot create user message from 'null' input")
                       .CreateModel(userMessage, Map);
        }

        private static ValidationResult<UserMessage> Map(UserMessageXml userMessage)
        {
            ValidationResult<UserMessage> CreateModel(UserMessageXml validated)
            {
                var transactionsResult = NonEmptyEnumerable<int>.Create(validated.Transactions);
                var propertiesResult =
                    validated.Properties.Traverse(MessageProperty.Create)
                             .SelectMany(ids => ids.AsUnique(p => p.Name));

                return ValidationResult.Combine(
                    transactionsResult,
                    propertiesResult,
                    (ts, ps) => new UserMessage(ts, ps));
            }

            return Spec.Of<UserMessageXml>()
                       .NotNull("cannot create user message from 'null' input")
                       .CreateModel(userMessage, CreateModel);
        }
    }

    public class ForwardJson
    {
        public string ReferenceId { get; set; }
    }

    public class Id
    {
        private Id(string value)
        {
            Value = value;
        }
        
        private string Value { get; }

        public static ValidationResult<Id> Create(string value)
        {
            return Spec.Of<string>("Id")
                       .NotNullOrWhiteSpace("Identifier should not be blank")
                       .LengthBetween(1, 100, "Identifier should have length between 1-100 inconclusive")
                       .Alphabetical("Identifier should have only elements of the A-Z range")
                       .CreateModel(value, x => new Id(x));
        }
    }
    
    public class Forward
    {
        private Forward(Id referenceId)
        {
            ReferenceId = referenceId;
        }
        
        public Id ReferenceId { get; }

        public static ValidationResult<Forward> Create(string referenceId)
        {
            return Id.Create(referenceId).Select(id => new Forward(id));
        }
    }
    
    public class ParameterJson
    {
        public string Key { get; set; }
        public string Value { get; set; }
    }

    public class Parameter
    {
        private Parameter(string key, string value)
        {
            Key = key;
            Value = value;
        }
        
        private string Key { get; }
        private string Value { get; }

        public static ValidationResult<Parameter> Create(string key, string value)
        {
            return Spec.Of<(string, string)>()
                       .NotNullOrWhiteSpace(kv => kv.Item1, "Key should not be blank")
                       .NotNullOrWhiteSpace(kv => kv.Item2, "Value should not be blank")
                       .CreateModel((key, value), kv => new Parameter(kv.Item1, kv.Item2));

        }
    }

    public class DeliverJson
    {
        public string Type { get; set; }
        
        public ParameterJson[] Parameters { get; set; }
    }

    public class Deliver
    {
        private Deliver(string type, NonEmptyEnumerable<Parameter> parameters)
        {
            Type = type;
            Parameters = parameters;
        }
        
        private string Type { get; }
        private NonEmptyEnumerable<Parameter> Parameters { get; }

        public static ValidationResult<Deliver> Create(string type, ParameterJson[] parameters)
        {
            var typeR =
                Spec.Of<string>()
                    .NotNullOrWhiteSpace("Type should not be blank")
                    .Validate(type);

            var parametersR =
                Spec.Of<ParameterJson>()
                    .NotNull("Parameter should not be null")
                    .ForArray()
                    .NonEmpty("Parameters should not be zero")
                    .CreateModel(parameters, xs => xs.Select(x => Parameter.Create(x.Key, x.Value)).Sequence())
                    .SelectMany(xs => xs.AsNonEmpty());

            return ValidationResult.Combine(typeR, parametersR, (t, ps) => new Deliver(t, ps));
        }
    }

    public class Target
    {
        private Target(FSharpChoice<Forward, Deliver> choice)
        {
            MessageHandling = choice;
        }
        
        public FSharpChoice<Forward, Deliver> MessageHandling { get; }

        public static ValidationResult<Target> Create(MessageHandlingJson handling)
        {
            return Spec.Of<MessageHandlingJson>()
                       .NotNull("Message handling should not be null")
                       .Add(h => h.IsDeliver || h.IsForwarding, "should either be deliver or forwarding")
                       .Add(h => (h.IsDeliver && h.IsForwarding) == false, "should either be deliver or forwarding")
                       .CreateModel(handling, h =>
                       {
                           if (h.IsDeliver)
                           {
                               return Deliver.Create(h.Deliver.Type, h.Deliver.Parameters)
                                             .Select(FSharpChoice<Forward, Deliver>.NewChoice2Of2);
                           }

                           return Forward.Create(h.Forward.ReferenceId)
                                         .Select(FSharpChoice<Forward, Deliver>.NewChoice1Of2);
                       })
                       .Select(c => new Target(c));
        }
    }
    
    public class MessageHandlingJson
    {
        [JsonIgnore]
        public bool IsForwarding => Forward != null;
        
        [JsonIgnore]
        public bool IsDeliver => Deliver != null;
        
        [JsonProperty("forward")]
        public ForwardJson Forward { get; set; }
        
        [JsonProperty("deliver")]
        public DeliverJson Deliver { get; set; }
    }
}
