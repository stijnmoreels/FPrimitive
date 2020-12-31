using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using FsCheck;
using FsCheck.Xunit;
using Xunit;

namespace FPrimitive.CSharp.Tests
{
    public class SpecTests
    {
        [Property]
        public Property Add(object target, bool result)
        {
            Spec<object> spec = Spec.Of<object>().Add(x => result, "should result in 'true'");
            return spec.ToProperty(target, result);
        }

        [Property]
        public Property Verify(object target, bool result)
        {
            Spec<object> spec = Spec.Of<object>().Add(x => (result, "should result in 'true'"));
            return spec.ToProperty(target, result);
        }

        [Property]
        public Property Equal(int left, int right)
        {
            Spec<int> spec = Spec.Of<int>().Equal(right, "should be equal");
            return spec.ToProperty(left, left == right);
        }

        [Property]
        public Property NotEqual(int left, int right)
        {
            Spec<int> spec = Spec.Of<int>().NotEqual(right, "should not be equal");
            return spec.ToProperty(left, left != right);
        }

        [Property]
        public Property NotNull()
        {
            Gen<string> nullOrNotNull =
                Arb.Generate<NonNull<string>>()
                   .Select(x => x.Get)
                   .OrNull();

            return Prop.ForAll(nullOrNotNull.ToArbitrary(), target => 
            { 
                Spec<string> spec = Spec.Of<string>().NotNull("should not be null");
                return spec.ToProperty(target, target != null);
            });
        }

        [Property]
        public Property NotNullOrEmptyString()
        {
            Gen<string> emptyOrNotEmpty =
                Arb.Generate<NonEmptyString>()
                   .Select(x => x.Get)
                   .OrNull();

            return Prop.ForAll(emptyOrNotEmpty.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().NotNullOrEmpty("should not be null or empty");
                return spec.ToProperty(target, !String.IsNullOrEmpty(target));
            });
        }

        [Property]
        public Property NotNullOrWhiteSpace()
        {
            Gen<string> nonEmptyString = Arb.Generate<NonEmptyString>().Select(x => x.Get);
            Gen<string> whiteSpace = Arb.Generate<PositiveInt>().Select(x => String.Join("", Enumerable.Repeat(" ", x.Get)));
            Gen<string> genString = nonEmptyString.Or(whiteSpace).OrNull();

            Gen<Func<Spec<string>>> genSpec = Gen.Constant<Func<Spec<string>>>(() => Spec.Of<string>().NotNullOrWhiteSpace("should not be null or whitespace"))
               .Or(Gen.Constant<Func<Spec<string>>>(() => Spec.Of<string>().NotNullOrEmpty("should not be null or empty").NotWhiteSpace("should not be whitespace")))
               .Or(Gen.Constant<Func<Spec<string>>>(() => Spec.Of<string>().NotNull("should not be null").NotEmpty("should not be empty").NotWhiteSpace("should nt be whitespace")));

            return Prop.ForAll(genString.ToArbitrary(), genSpec.ToArbitrary(), (target, getSpec) =>
            {
                Spec<string> spec = getSpec();
                return spec.ToProperty(target, !String.IsNullOrWhiteSpace(target));
            });
        }

        [Property]
        public Property StartsWith(NonEmptyString prefix, NonNull<string> remaining)
        {
            Gen<string> gen = Gen.Elements(prefix.Get + remaining.Get, remaining.Get);
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().StartsWith(prefix.Get, "should start with prefix");
                return spec.ToProperty(target, target.StartsWith(prefix.Get));
            });
        }

        [Property]
        public Property EndsWith(NonNull<string> remaining, NonEmptyString suffix)
        {
            Gen<string> gen = Gen.Elements(remaining.Get + suffix.Get, remaining.Get);
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().EndsWith(suffix.Get, "should end with suffix");
                return spec.ToProperty(target, target.EndsWith(suffix.Get));
            });
        }

        [Property]
        public Property SequenceEqual(NonNull<int[]> array)
        {
            Gen<int[]> gen = Gen.Constant(array.Get).Or(Gen.Shuffle(array.Get)).Or(Arb.Generate<int[]>());
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().SequenceEqual(array.Get, "should be sequence eaual");
                return spec.ToProperty(target, target.SequenceEqual(array.Get));
            });
        }

        [Property]
        public Property NonEmpty(NonEmptyArray<string> array)
        {
            Gen<string[]> gen = Gen.Constant(Array.Empty<string>()).Or(Gen.Constant(array.Get));
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string[]> spec = Spec.Of<string[]>().NonEmpty("should not be empty");
                return spec.ToProperty(target, target.Length != 0);
            });
        }

        [Property]
        public Property All(NonEmptyArray<PositiveInt> array)
        {
            Gen<int[]> gen = Gen.Constant(Array.Empty<int>())
               .Or(Gen.Constant(array.Get.Select(x => x.Get).ToArray()))
               .Or(Arb.Generate<int[]>());

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().All(x => x > 0, "all should be greater than zero");
                return spec.ToProperty(target, target.All(x => x > 0));
            });
        }

        [Property]
        public Property None(NonEmptyArray<FsCheck.NonZeroInt> array)
        {
            Gen<int[]> gen = Gen.Constant(array.Get.Select(x => x.Get).ToArray())
               .Or(Arb.Generate<PositiveInt>().Select(x => Enumerable.Repeat(0, x.Get).ToArray()))
               .Or(Gen.Constant(Array.Empty<int>()))
               .Or(Arb.Generate<int[]>());

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().None(x => x == 0, "none should be zero");
                return spec.ToProperty(target, target.All(x => x != 0));
            });
        }

        [Property]
        public Property Sequence_Contains(int x, int[] xs)
        {
            Gen<int[]> gen = Gen.Constant(xs.Append(x).ToArray())
                .Or(Arb.Generate<PositiveInt>().Select(i => Enumerable.Repeat(x, i.Get).ToArray()))
                .Or(Gen.Constant(Array.Empty<int>()))
                .Or(Arb.Generate<int[]>());

            return Prop.ForAll(gen.ToArbitrary(), target => 
            {
                Spec<int[]> spec = Spec.Of<int[]>().Contains(x, "should contain element");
                return spec.ToProperty(target, target.Contains(x));
            });
        }

        [Property]
        public Property Sequence_ContainsAll(int[] xs, int[] ys)
        {
            Gen<int[]> gen = Gen.Constant(xs.Concat(ys).ToArray())
               .Or(Gen.Constant(xs))
               .Or(Gen.Constant(ys))
               .Or(Arb.Generate<int[]>())
               .Or(Gen.Constant(Array.Empty<int>()));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().ContainsAll(ys, "should contain all");
                return spec.ToProperty(target, ys.All(target.Contains));
            });
        }

        [Property]
        public Property String_Contains(NonEmptyString substring, string remaining)
        {
            Gen<string> gen = Gen.Shuffle(new[] { substring.Get, remaining })
               .Select(strings => String.Join(String.Empty, strings))
               .Or(Arb.Generate<string>())
               .Or(Gen.Constant(substring.Get));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().Contains(substring.Get, "should contain substring");
                return spec.ToProperty(target, target?.Contains(substring.Get) == true);
            });
        }

        [Property]
        public Property String_NotContains(NonEmptyString substring, NonNull<string> remaining)
        {
            Gen<string> gen = Gen.Shuffle(new[] { substring.Get, remaining.Get })
               .Select(strings => String.Join(String.Empty, strings))
               .Or(Arb.Generate<NonNull<string>>().Select(s => s.Get))
               .Or(Gen.Constant(substring.Get));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().NotContains(substring.Get, "should not contain substring");
                return spec.ToProperty(target, !target.Contains(substring.Get));
            });
        }

        [Property(Replay = "467209361, 296796389")]
        public Property String_ContainsAll(NonEmptyArray<NonEmptyString> nonEmptySubstrings, NonEmptyString remaining)
        {
            IEnumerable<string> substrings = nonEmptySubstrings.Get.Select(s => s.Get);
            Gen<string> gen = Gen.Shuffle(substrings.Append(remaining.Get))
               .Select(strings => String.Join(String.Empty, strings))
               .Or(Arb.Generate<NonNull<string>>().Select(s => s.Get))
               .Or(Gen.Constant(String.Join(String.Empty, substrings)));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().ContainsAll(substrings, "should contain substring");
                return spec.ToProperty(target, substrings.All(target.Contains));
            }).When(substrings.All(s => s != remaining.Get));
        }

        [Property]
        public Property Any(PositiveInt item, NegativeInt[] xs)
        {
            Gen<int[]> gen = Gen.Shuffle(xs.Select(x => x.Get).Append(item.Get))
               .Or(Arb.Generate<NegativeInt>().Select(x => x.Get).ArrayOf())
               .Or(Gen.Constant(Array.Empty<int>()));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().Any(x => x > 0, "one should be greater than zero");
                return spec.ToProperty(target, target.Any(x => x > 0));
            });
        }

        [Property]
        public Property NotAny(PositiveInt item, NegativeInt[] xs)
        {
            Gen<int[]> gen = Gen.Shuffle(xs.Select(x => x.Get).Append(item.Get))
               .Or(Arb.Generate<NegativeInt>().Select(x => x.Get).ArrayOf())
               .Or(Gen.Constant(Array.Empty<int>()));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().NotAny(x => x > 0, "one should be greater than zero");
                return spec.ToProperty(target, target.All(x => x < 0));
            });
        }

        [Property]
        public Property Single(PositiveInt item, NegativeInt[] xs)
        {
            Gen<int[]> gen = Gen.Shuffle(xs.Select(x => x.Get).Append(item.Get))
               .Or(Arb.Generate<NegativeInt>().Select(x => x.Get).ArrayOf())
               .Or(Arb.Generate<PositiveInt>().Select(x => x.Get).ArrayOf())
               .Or(Gen.Constant(Array.Empty<int>()));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().Single(x => x > 0, "one should be greater than zero");
                return spec.ToProperty(target, target.Count(x => x > 0) == 1);
            });
        }

        [Property]
        public Property Unique(PositiveInt x, PositiveInt y, PositiveInt z)
        {
            Spec<PositiveInt[]> spec = Spec.Of<PositiveInt[]>().Unique(i => i.Get, "should only include unique items");
            return spec.ToProperty(new[] { x, y, z })
                       .When(x.Get != y.Get && x.Get != z.Get && y.Get != z.Get)
                       .And(spec.ToProperty(Array.Empty<PositiveInt>()));
        }

        [Property]
        public Property UniqueItems(PositiveInt x, PositiveInt y, PositiveInt z)
        {
            Spec<PositiveInt[]> spec = Spec.Of<PositiveInt[]>().Unique("should only include unique items");
            return spec.ToProperty(new[] { x, y, z })
                       .When(x.Get != y.Get && x.Get != z.Get && y.Get != z.Get)
                       .And(spec.ToProperty(Array.Empty<PositiveInt>()));
        }

        [Property]
        public Property NotUnique(NegativeInt item)
        {
            Gen<int[]> gen = Arb.Generate<NonEmptyArray<int>>()
                .Select(x => x.Get)
                .Where(xs => xs.GroupBy(x => x).Any(g => g.Count() > 1))
                .Or(Gen.Constant(item.Get).ArrayOf().Where(xs => xs.Length > 1));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().Unique(x => x + 1, "should include only unique items");
                return spec.ToProperty(target, expected: false);
            });
        }

        [Property]
        public Property NotUniqueItems(NegativeInt item)
        {
            Gen<int[]> gen = Arb.Generate<NonEmptyArray<int>>()
                .Select(x => x.Get)
                .Where(xs => xs.GroupBy(x => x).Any(g => g.Count() > 1))
                .Or(Gen.Constant(item.Get).ArrayOf().Where(xs => xs.Length > 1));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<int[]> spec = Spec.Of<int[]>().Unique("should include only unique items");
                return spec.ToProperty(target, expected: false);
            });
        }

        [Property]
        public Property Structure(PositiveInt x, NegativeInt y, FsCheck.NonZeroInt z)
        {
            Spec<int[]> spec = Spec.Of<int[]>()
                .Structure(new Func<int, bool>[]
                {
                    i => i > 0,
                    i => i < 0,
                    i => i != 0
                }, "should match the structure");

            return spec.ToProperty(new [] { x.Get, y.Get, z.Get });
        }

        [Property]
        public Property NotStructureLength(string input)
        {
            Spec<string[]> spec = Spec.Of<string[]>()
                .Structure(new Func<string, bool>[]
                {
                    s => s != String.Empty,
                    s => s != "some string",
                }, "should match structure");

            return spec.ToProperty(new[] { input }, expected: false);
        }

        [Property]
        public Property Length(object[] xs)
        {
            Gen<object[]> gen = 
                Gen.Constant(xs)
                   .Or(Arb.Generate<object[]>());

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<object[]> spec = Spec.Of<object[]>().Length(xs.Length, "should match length");
                return spec.ToProperty(target, xs.Length == target.Length);
            });
        }

        [Property]
        public Property LengthBetween(object[] xs)
        {
            Gen<object[]> gen =
                Gen.Constant(xs)
                   .Or(Arb.Generate<object[]>());

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<object[]> spec = 
                    Spec.Of<object[]>()
                        .LengthBetween(xs.Length - 1, xs.Length + 1, "length should between expected range");
                
                return spec.ToProperty(target, 
                    xs.Length - 1 <= target.Length && target.Length <= xs.Length + 1);
            });
        }

        [Property]
        public Property LengthMax(object[] xs)
        {
            Gen<object[]> gen = 
                Gen.Constant(xs)
                   .Or(Arb.Generate<object[]>());

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<object[]> spec = Spec.Of<object[]>().LengthMax(xs.Length + 1, "should be less or equal to max length");
                return spec.ToProperty(target, target.Length <= xs.Length + 1);
            });
        }

        [Property]
        public Property LengthMin(object[] xs, object[] ys)
        {
            Gen<object[]> gen =
                Gen.Constant(xs)
                   .Or(Gen.Constant(ys));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<object[]> spec = Spec.Of<object[]>().LengthMin(xs.Length - 1, "should be at least min length");
                return spec.ToProperty(target, target.Length >= xs.Length - 1);
            });
        }

        [Property]
        public Property LessThan(int target)
        {
            Spec<int> spec = Spec.Of<int>().LessThan(0, "should be less than zero");
            return spec.ToProperty(target, target < 0);
        }

        [Property]
        public Property GreaterThan(int target)
        {
            Spec<int> spec = Spec.Of<int>().GreaterThan(0, "should be greater than zero");
            return spec.ToProperty(target, target > 0);
        }

        [Property]
        public Property LessThanOrEqual(int target)
        {
            Spec<int> spec = Spec.Of<int>().LessThanOrEqual(0, "should be less than or equal to zero");
            return spec.ToProperty(target, target <= 0);
        }

        [Property]
        public Property GreaterThanOrEqual(int target)
        {
            Spec<int> spec = Spec.Of<int>().GreaterThanOrEqual(0, "should be greater or equal to zero");
            return spec.ToProperty(target, target >= 0);
        }

        [Property]
        public Property String_Length(NonNull<string> s, NonNull<string> x)
        {
            Gen<string> gen = Gen.Constant(s.Get).Or(Gen.Constant(x.Get));
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().Length(s.Get.Length, "should have specific string length");
                return spec.ToProperty(target, target.Length == s.Get.Length);
            });
        }

        [Property]
        public Property String_LengthMin(NonNull<string> s, NonNull<string> x)
        {
            Gen<string> gen = Gen.Constant(s.Get).Or(Gen.Constant(x.Get));
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().LengthMin(s.Get.Length - 1, "should have min string length");
                return spec.ToProperty(target, target.Length >= s.Get.Length - 1);
            });
        }

        [Property]
        public Property String_LengthMax(NonNull<string> s, NonNull<string> x)
        {
            Gen<string> gen = Gen.Constant(s.Get).Or(Gen.Constant(x.Get));
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().LengthMax(s.Get.Length + 1, "should have max string length");
                return spec.ToProperty(target, target.Length <= s.Get.Length + 1);
            });
        }

        [Property]
        public Property String_LengthBetween(NonEmptyString s)
        {
            Gen<(int min, int max)> gen = 
                Gen.Choose(1, s.Get.Length)
                   .SelectMany(min => Gen.Choose(min, Int32.MaxValue).Select(max => (min, max)));

            return Prop.ForAll(gen.ToArbitrary(), range => 
            {
                Spec<string> spec = Spec.Of<string>().LengthBetween(range.min, range.max, "should between string length min/max");
                return spec.ToProperty(s.Get);
            });
        }

        [Property]
        public Property InclusiveBetween(PositiveInt x)
        {
            Gen<Interval> gen =
                Gen.Constant(Interval.NewInterval(1, Int32.MaxValue))
                   .Or(Arb.Generate<Interval>());

            return Prop.ForAll(gen.ToArbitrary(), interval =>
            {
                Spec<int> spec = Spec.Of<int>().InclusiveBetween(interval.Left, interval.Right, "should between interval inconclusive");
                return spec.ToProperty(x.Get, x.Get >= interval.Left && x.Get <= interval.Right);
            });
        }

        [Property]
        public Property ExclusiveBetween(PositiveInt x)
        {
            Gen<Interval> gen =
                Gen.Constant(Interval.NewInterval(0, Int32.MaxValue - 1))
                   .Or(Arb.Generate<Interval>());

            return Prop.ForAll(gen.ToArbitrary(), interval =>
            {
                Spec<int> spec = Spec.Of<int>().ExclusiveBetween(interval.Left, interval.Right, "should between interval inconclusive");
                return spec.ToProperty(x.Get, x.Get > interval.Left && x.Get < interval.Right);
            });
        }

        [Property]
        public Property Matches(PositiveInt[] xs)
        {
            IEnumerable<string> inputs =
                Enumerable.Range('A', 26)
                          .Select(i => ((char) i).ToString())
                          .Concat(xs.Select(x => x.Get.ToString()));
            
            Gen<string> gen = 
                Gen.Elements(inputs)
                   .NonEmptyListOf()
                   .Select(parts => String.Join("", parts));

            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().Matches("^[A-Z0-9]+$", "should match pattern");
                return spec.ToProperty(target);
            });
        }

        [Property]
        public Property Alphabetical()
        {
            IEnumerable<string> inputs = 
                Enumerable.Range('A', 26)
                          .Concat(Enumerable.Range('a', 26))
                          .Select(s => s.ToString());
            
            Gen<string> gen = 
                Gen.Elements(inputs)
                   .NonEmptyListOf()
                   .Select(s => String.Join("", s));
            
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().Alphabetical("should only contain alphabetical chars");
                return spec.ToProperty(target);
            });
        }

        [Property]
        public Property Alphanum(PositiveInt[] numbers)
        {
            IEnumerable<string> inputs = 
                Enumerable.Range('A', 26)
                          .Concat(Enumerable.Range('a', 26))
                          .Select(c => c.ToString())
                          .Concat(numbers.Select(n => n.Get.ToString()));
            
            Gen<string> gen = 
                Gen.Elements(inputs)
                   .NonEmptyListOf()
                   .Select(s => String.Join("", s));
            
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().Alphanum("should only contain alphabetical chars and numbers");
                return spec.ToProperty(target);
            });
        }

        [Property]
        public Property AlphanumSpecial(PositiveInt[] numbers)
        {
            IEnumerable<string> inputs = 
                Enumerable.Range('A', 26)
                          .Concat(Enumerable.Range('a', 26))
                          .Select(c => c.ToString())
                          .Concat(numbers.Select(n => n.Get.ToString()))
                          .Concat(new[] { "#", "%", "(" });
            
            Gen<string> gen = 
                Gen.Elements(inputs)
                   .NonEmptyListOf()
                   .Select(s => String.Join("", s));
            
            return Prop.ForAll(gen.ToArbitrary(), target =>
            {
                Spec<string> spec = Spec.Of<string>().AlphanumSpecial("should only contain alphabetical chars and numbers");
                return spec.ToProperty(target);
            });
        }

        [Property]
        public Property IsTypeT(PositiveInt x)
        {
            Spec<object> spec = Spec.Of<object>().IsType<object, PositiveInt>("should be of type 'positive int'");
            return spec.ToProperty(x);
        }

        [Property]
        public Property IsType(NegativeInt x)
        {
            Spec<object> spec = Spec.Of<object>().IsType(typeof(NegativeInt), "should be of type 'negative int'");
            return spec.ToProperty(x);
        }

        [Property]
        public void Cascade(NegativeInt x)
        {
            ValidationResult<int> result =
                Spec.Of<int>("int")
                    .GreaterThan(0, "should be greater than zero")
                    .Equal(0, "should be equal to zero")
                    .Cascade(CascadeMode.Continue)
                    .Validate(x.Get);

            Assert.Equal(1, result.Details.Count);
            Assert.True(result.Details.TryGetValue("int", out string[] errors));
            Assert.Equal(2, errors.Length);
        }
    }

    public static class SpecExtensions
    {
        public static Property ToProperty<T>(this Spec<T> spec, T target, bool expected = true)
        {
            ValidationResult<T> result = spec.Validate(target);
            if (result.IsValid == expected)
            {
                return true.ToProperty();
            }

            string values = String.Join(", ", result.Details.Values.SelectMany(v => v));
            string label = String.Join(Environment.NewLine, values); 
            return false.ToProperty().Label(label);
        }
    }
}
