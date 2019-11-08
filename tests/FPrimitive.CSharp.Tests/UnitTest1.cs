using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Net.Mail;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Schema;
using Microsoft.FSharp.Core;
using FPrimitive;
using Microsoft.FSharp.Control;
using Newtonsoft.Json;

namespace FPrimitive.CSharp.Tests
{
    public class UnitTest1
    {
        public void Test1<T>()
        {
            // Specification before the actual check of the base-uri.
            Spec<Uri> spec =
                Spec.Of<Uri>()
                    .Add(uri => uri.Scheme == Uri.UriSchemeHttps, "should be 'https' scheme");

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
                       .Regex(pattern, $"ebMS string can only contain chars that matches {pattern}")
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
            return Spec.Of<string>()
                       .NotNull("Product name should not be 'null'")
                       .NotEmpty("Product name should not be empty")
                       .NotWhiteSpace("Product name should not contain only white-space characters")
                       .Regex("^[a-zA-Z ]+$", "Product name should only contain characters from a-z")
                       .Add(input => input.Length == 20, "Product name should only have a length of 20")
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
        public Party(UniqueSeq<NonWhiteSpaceString> ids)
        {
            Ids = ids;
        }

        public UniqueSeq<NonWhiteSpaceString> Ids { get; }

        public static ValidationResult<Party> Create(IEnumerable<string> ids)
        {
            return Spec.Of<IEnumerable<string>>()
                       .NotNull("unique ID sequence cannot be null")
                       .All(x => !(x is null), "individual ID's cannot be null")
                       .CreateModel(ids, xs => xs.Traverse(NonWhiteSpaceString.Create))
                       .Then(UniqueSeq<NonWhiteSpaceString>.Create)
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
            const string pattern = @"^[a-zA-Z\.]+$";
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("author name should not be blank")
                       .Add(s => s.Length > 1, "author name should at least be a single character")
                       .Matches(pattern, $"author name should match regular expression: {pattern}")
                       .CreateModel(author, validated => new Author(validated));
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
            const string pattern = "^[0-9]$";
            return Spec.Of<string>()
                       .NotNullOrWhiteSpace("ISBN13 number should not be blank")
                       .Add(s => s.Length == 13, "ISBN13 number should be 13 characters long")
                       .Matches(pattern, $"ISBN13 number should match regular expression: {pattern}")
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
                       .Select((i, n) => i % 2 != 0 ? n * 3 : n)
                       .Sum();

            int remaining = sumNumbers % 10;
            int checksum = remaining != 0 ? 10 - remaining : remaining;

            int lastNumber = numbers.Last();
            return checksum == lastNumber;
        }
    }

    public class Book
    {
        private readonly Author _author;
        private readonly ISBN13 _isbn13;

        private Book(Author author, ISBN13 isbn13)
        {
            _author = author;
            _isbn13 = isbn13;
        }

        public static ValidationResult<Book> Create(string author, string isbn13)
        {
            ValidationResult<Author> authorResult = Author.Create(author);
            ValidationResult<ISBN13> isbn13Result = ISBN13.Create(isbn13);

            return ValidationResult.Combine(authorResult, isbn13Result, (auth, isbn) => new Book(auth, isbn));
        }
    }

    public class BookJson
    {
        [JsonProperty("author")]
        public string Author { get; set; }

        [JsonProperty("isbn13")]
        public string ISBN13 { get; set; }
    }
}
