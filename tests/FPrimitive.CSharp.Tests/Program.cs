using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Newtonsoft.Json.Linq;

namespace FPrimitive.CSharp.Tests
{
    class Program
    {
        public static void Main(string[] args)
        {
            Spec<Uri> spec =
                Spec.Of<Uri>()
                    .Add(uri => uri.Scheme == Uri.UriSchemeHttps, "should be 'https' scheme");

            ValidationResult<Uri> validationResult = spec.Validate(new Uri("http://localhost", UriKind.Absolute));

            //string json = @"{ ""author"": ""Philip K. Dick"", ""isbn13"": ""978-0-575-07921-2"", ""pages"": 202 }";

            // loading
            //string json = @"{ 'product': 'Microsoft Surface Pro 6', 'price': '729' }";
            //string json = "åß∂ƒ©˙∆˚¬…æœ∑®†¥¨ˆøπ“‘¡™£¢∞§¶•ªº–≠";

            // syntax
            //string json = @"{ ""author"": """", ""title"": """", ""isbn13"": ""978-0-575-07921-2"", ""pages"": 1001 }";
            string json = "{ 'isbn13': 9780575079212, 'pages': '202' }";

            var result =
                Json.Load(json)
                    .SelectMany(loaded => Json.Validate(loaded, BookJson.Schema));
                    //.SelectMany(validated => Json.Deserialize<BookJson>(validated))
                    //.SelectMany(dto => Map.ToModel(dto));

            if (result.TryGetValue(out var value))
            {
                Console.WriteLine("Success: " + value);
            }
            else
            {
                Console.Error.WriteLine("Failure: " + String.Join(", ", result.Error));
            }
        }
    }
}
