using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Net.Mail;
using System.Text;
using Microsoft.FSharp.Core;
using FPrimitive;

namespace FPrimitive.CSharp.Tests
{
    public class ProductName
    {
        private ProductName(string value)
        {
            Value = Spec.Of<string>()
                        .NotNull("Product name should not be 'null'")
                        .NotEmpty("Product name should not be empty")
                        .NotWhiteSpace("Product name should not contain only white-space characters")
                        .Regex("^[a-zA-Z ]+$", "Product name should only contain characters from a-z")
                        .ValidateThrow<string, ArgumentException>(value, "");
        }

        public string Value { get; }

        public static ValidationResult<ProductName> Create(string untrusted)
        {
            return Spec.Of<string>()
                       .NotNull("Product name should not be 'null'")
                       .NotEmpty("Product name should not be empty")
                       .NotWhiteSpace("Product name should not contain only white-space characters")
                       .Regex("^[a-zA-Z ]+$", "Product name should only contain characters from a-z")
                       .Add(input => input.Length == 20, "Product name should only have a length of 20")
                       .Cascade(CascadeMode.FirstFailure)
                       .CreateModel(untrusted, validated => new ProductName(validated));
        }
    }


    public class UnitTest1
    {
        public void Test1()
        {
            Access<Unit, int> access = 
                Access.Function(() => 1)
                      .Once()
                      .DuringHours(12, 13)
                      .Revokable();

            access.Revoke();
            AccessResult<int> result = access.Eval();

            Untrust<int> five = 5;

            Access<FileInfo, FileInfo> revokable =
                Access.OnlyFilesFromDirectory(new DirectoryInfo(""))
                      .Extension(".txt")
                      .Times(3)
                      .DuringHours(2, 10)
                      .Revokable();

            AccessResult<FileInfo> accessResult = revokable.Eval(new FileInfo("txt"));
            if (accessResult.TryGetValue(out FileInfo output))
            {
                
            }
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
}
