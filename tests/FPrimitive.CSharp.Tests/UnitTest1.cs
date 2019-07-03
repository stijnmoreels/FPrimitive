// using System;
// using System.Collections;
// using System.Collections.Generic;
// using System.IO;
// using System.Net.Mail;
// using System.Text;
// using System.Xml;
// using Microsoft.FSharp.Core;
// using FPrimitive;
// using Microsoft.FSharp.Control;

// namespace FPrimitive.CSharp.Tests
// {
//     public class UnitTest1
//     {
//         public void Test1<T>()
//         {
//             // Specification before the actual check of the base-uri.
//             Spec<Uri> spec = 
//                 Spec.Of<Uri>()
//                     .Add(uri => uri.Scheme == Uri.UriSchemeHttps, "should be 'https' scheme");

//             // Demonstration purposes, generic type could be anything.
//             IObservable<T> obs = null;

//             // Access controlled function with validation, revocation, limited amount of evaluations and time-based availability.
//             Access<Uri, Uri> access =
//                 Access.OnlyUriFromBase(new Uri("https://localhost:9090/"))
//                       .Matching(spec)
//                       .Once()
//                       .DuringHours(9, 17)
//                       .RevokedWhen(obs);


//             // Somewhere else...
//             AccessResult<Uri> result = access.Eval(new Uri("http://localhost:9090/path"));
//             if (result.TryGetValue(out Uri output))
//             {
//                 // use the now correct 'output'
//             }

            
//         }
//     }

//     public class ReadableText
//     {
//         private readonly string _value;
//         private ReadableText(string name)
//         {
//             _value = name;
//         }

//         public static ValidationResult<ReadableText> Create(string untrusted)
//         {
//             return Spec.Of<string>()
//                        .NotNull("Product name should not be 'null'")
//                        .NotEmpty("Product name should not be empty")
//                        .NotWhiteSpace("Product name should not contain only white-space characters")
//                        .Regex("^[a-zA-Z ]+$", "Product name should only contain characters from a-z")
//                        .Add(input => input.Length == 20, "Product name should only have a length of 20")
//                        .Cascade(CascadeMode.FirstFailure)
//                        .CreateModel(untrusted, validated => new ReadableText(validated));
//         }
//     }

//     public class Int1To20
//     {
//         private readonly int _value;
//         private Int1To20(int value)
//         {
//             _value = value;
//         }

//         public static ValidationResult<Int1To20> Create(int untrusted)
//         {
//             return Spec.Of<int>()
//                        .InclusiveBetween(1, 20, "Integer should be between 1-20")
//                        .Cascade(CascadeMode.Continue)
//                        .CreateModel(untrusted, validated => new Int1To20(validated));
//         }
//     }

//     public class Record
//     {
//         private Record(ReadableText name, Int1To20 amount)
//         {
//             ProductName = name;
//             Amount = amount;
//         }

//         public ReadableText ProductName { get; }
//         public Int1To20 Amount { get; }

//         public static ValidationResult<Record> Create(string productName, int amount)
//         {
//             return ValidationResult<Record>.Combine(
//                 ReadableText.Create(productName),
//                 Int1To20.Create(amount),
//                 (txt, integer) => new Record(txt, integer));
//         }
//     }

//     public class Name
//     {
//         /// <summary>
//         /// Initializes a new instance of the <see cref="Name"/> class.
//         /// </summary>
//         private Name(string value)
//         {
//             Value = value;
//         }

//         public string Value { get; }

//         // public ValidationResult<Name> Create(string value)
//         // {
//         //     return Spec.Of<string>()
//         //         .NotNull("Name should not be 'null'")
//         //         .NotEmpty("Name should not be empty")
//         //         .CreateModel(value, x => new Name(x));
//         // }

//         // public static Name CreateOrThrow(string value)
//         // {
//         //     return Spec.Of<string>()
//         //         .NotNull("Name should not be 'null'")
//         //         .NotEmpty("Name should not be empty")
//         //         .Cascade(CascadeMode.Continue)
//         //         .CreateModelOrThrow(value, x => new Name(x), "Cannot create a Name instance");
//         // }

//         // public static bool TryCreate(string value, out Name output)
//         // {
//         //     return Spec.Of<string>()
//         //         .NotNull("Name should not be 'null'")
//         //         .NotEmpty("Name should not be empty")
//         //         .TryCreateModel(value, x => new Name(x), out output);
//         // }
//     }

//     public class Point
//     {
//         private Point(int min, int max)
//         {
//             Min = min;
//             Max = max;
//         }

//         public int Min { get; }
//         public int Max { get; }

//         // public static Point Create(int min, int max)
//         // {
//         //     return Spec.Of<(int min, int max)>()
//         //         .InclusiveBetween(x => x.min, 0, 10, "Minimum should be between 1-10")
//         //         .InclusiveBetween(x => x.max, 11, 20, "Maximum should be between 11-20")
//         //         .CreateModelOrThrow((min: min, max: max), x => new Point(x.min, x.max), "Could not create Point instance");
//         // }
//     }
// }
