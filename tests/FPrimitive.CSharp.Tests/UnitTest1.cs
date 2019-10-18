using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Net.Mail;
using System.Text;
using System.Xml;
using Microsoft.FSharp.Core;
using FPrimitive;
using Microsoft.FSharp.Control;

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
            Min = min;
            Max = max;
        }

        public int Min { get; }
        public int Max { get; }

        // public static Point Create(int min, int max)
        // {
        //     return Spec.Of<(int min, int max)>()
        //         .InclusiveBetween(x => x.min, 0, 10, "Minimum should be between 1-10")
        //         .InclusiveBetween(x => x.max, 11, 20, "Maximum should be between 11-20")
        //         .CreateModelOrThrow((min: min, max: max), x => new Point(x.min, x.max), "Could not create Point instance");
        // }
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
}
