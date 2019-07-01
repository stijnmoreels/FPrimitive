using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

// ReSharper disable once CheckNamespace
namespace Microsoft.FSharp.Core 
{
    [Serializable]
    [CompiledName("Option`1")]
    [DebuggerDisplay("Some({" + nameof(Value) + "})")]
    public class Option<T> 
        : IEquatable<Option<T>>, 
          IEquatable<FSharpOption<T>>, 
          IStructuralEquatable, 
          IComparable<Option<T>>,
          IComparable<FSharpOption<T>>,
          IComparable, 
          IStructuralComparable
    {
        private readonly FSharpOption<T> _option;

        private Option(FSharpOption<T> option)
        {
            _option = option;
            IsSome = OptionModule.IsSome(option);
        }

        public bool IsSome { get; }

        public T Value => _option.Value;

        public static Option<T> None { get; } = new Option<T>(FSharpOption<T>.None);

        public static Option<T> Some(T value)
        {
            return new Option<T>(new FSharpOption<T>(value));
        }

        /// <summary>Indicates whether the current object is equal to another object of the same type.</summary>
        /// <param name="other">An object to compare with this object.</param>
        /// <returns>true if the current object is equal to the <paramref name="other">other</paramref> parameter; otherwise, false.</returns>
        public bool Equals(Option<T> other)
        {
            if (other is null)
            {
                return false;
            }

            if (ReferenceEquals(this, other))
            {
                return true;
            }

            return _option.Equals(other._option);
        }

        public int CompareTo(Option<T> other)
        {
            return _option.CompareTo(other._option);
        }

        /// <summary>Indicates whether the current object is equal to another object of the same type.</summary>
        /// <param name="other">An object to compare with this object.</param>
        /// <returns>true if the current object is equal to the <paramref name="other">other</paramref> parameter; otherwise, false.</returns>
        public bool Equals(FSharpOption<T> other)
        {
            return _option.Equals(other);
        }

        public int CompareTo(FSharpOption<T> other)
        {
            return _option.CompareTo(other);
        }

        /// <summary>Determines whether the specified object is equal to the current object.</summary>
        /// <param name="obj">The object to compare with the current object.</param>
        /// <returns>true if the specified object  is equal to the current object; otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            if (obj is null)
            {
                return false;
            }

            if (ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj is Option<T> other)
            {
                return Equals(other);
            }

            if (obj is FSharpOption<T> otherFSharp)
            {
                return _option.Equals(otherFSharp);
            }

            return false;

        }

        public int CompareTo(object obj, IComparer comparer)
        {
            if (obj is Option<T> other)
            {
                return _option.CompareTo(other._option, comparer);
            }
            if (obj is FSharpOption<T> otherFSharp)
            {
                return _option.CompareTo(otherFSharp, comparer);
            }
            else
            {
                return 0;
            }
        }

        public int CompareTo(object obj)
        {
            if (obj is Option<T> other)
            {
                return _option.CompareTo(other._option);
            }
            else if (obj is FSharpOption<T> otherFSharp)
            {
                return _option.CompareTo(otherFSharp);
            }
            else
            {
                return 0;
            }
        }

        /// <summary>Determines whether an object is structurally equal to the current instance.</summary>
        /// <param name="obj">The object to compare with the current instance.</param>
        /// <param name="comparer">An object that determines whether the current instance and other are equal.</param>
        /// <returns>true if the two objects are equal; otherwise, false.</returns>
        public bool Equals(object obj, IEqualityComparer comparer)
        {
            if (obj is null)
            {
                return false;
            }

            if (ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj is Option<T> other)
            {
                return _option.Equals(other._option, comparer);
            }

            if (obj is FSharpOption<T> otherFSharp)
            {
                return _option.Equals(otherFSharp, comparer);
            }

            return false;

        }

        /// <summary>Returns a value that indicates whether the values of two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects are equal.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if the <paramref name="left" /> and <paramref name="right" /> parameters have the same value; otherwise, false.</returns>
        public static bool operator ==(Option<T> left, Option<T> right)
        {
            return Equals(left, right);
        }

        /// <summary>Returns a value that indicates whether two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects have different values.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator !=(Option<T> left, Option<T> right)
        {
            return !Equals(left, right);
        }

        /// <summary>Returns a value that indicates whether two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects have different values.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator ==(Option<T> left, FSharpOption<T> right)
        {
            FSharpOption<T> option = left;
            return Equals(option, right);
        }

        /// <summary>Returns a value that indicates whether two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects have different values.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator !=(Option<T> left, FSharpOption<T> right)
        {
            FSharpOption<T> option = left;
            return !Equals(option, right);
        }

        /// <summary>Returns a value that indicates whether the values of two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects are equal.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if the <paramref name="left" /> and <paramref name="right" /> parameters have the same value; otherwise, false.</returns>
        public static bool operator ==(FSharpOption<T> left, Option<T> right)
        {
            FSharpOption<T> other = right;
            return Equals(left, other);
        }

        /// <summary>Returns a value that indicates whether two <see cref="T:Microsoft.FSharp.Core.Option`1" /> objects have different values.</summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator !=(FSharpOption<T> left, Option<T> right)
        {
            FSharpOption<T> other = right;
            return !Equals(left, other);
        }

        /// <summary>Serves as the default hash function.</summary>
        /// <returns>A hash code for the current object.</returns>
        public override int GetHashCode()
        {
            return _option.GetHashCode();
        }

        /// <summary>Returns a hash code for the current instance.</summary>
        /// <param name="comparer">An object that computes the hash code of the current object.</param>
        /// <returns>The hash code for the current instance.</returns>
        public int GetHashCode(IEqualityComparer comparer)
        {
            return _option.GetHashCode(comparer);
        }

        public static implicit operator FSharpOption<T>(Option<T> option)
        {
            return option._option;
        }

        public static implicit operator Option<T>(FSharpOption<T> option)
        {
            return new Option<T>(option);
        }

        /// <summary>Returns a string that represents the current object.</summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return _option.ToString();
        }
    }

    public static class Option
    {
        public static Option<T> Some<T>(T value)
        {
            if (value == null)
            {
                throw new ArgumentNullException(nameof(value));
            }

            return Option<T>.Some(value);
        }

        public static Option<T> None<T>()
        {
            return Option<T>.None;
        }

        public static Option<T> SomeOrNone<T>(T value) where T : class
        {
            return OptionModule.OfObj(value);
        }
    }

    public static class OptionExtensions
    {
        public static Option<TResult> Select<T, TResult>(this Option<T> option, Func<T, TResult> selector)
        {
            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return option.IsSome
                ? Option<TResult>.Some(selector(option.Value))
                : Option<TResult>.None;
        }

        public static Option<TResult> Select<T, TResult>(this FSharpOption<T> option, Func<T, TResult> selector)
        {
            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return OptionModule.IsSome(option)
                ? Option<TResult>.Some(selector(option.Value))
                : Option<TResult>.None;
        }

        public static Option<TResult> SelectMany<T, TResult>(
            this Option<T> option,
            Func<T, Option<TResult>> selector)
        {
            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return option.IsSome
                ? selector(option.Value)
                : Option<TResult>.None;
        }

        public static Option<TResult> SelectMany<T, TResult>(
            this FSharpOption<T> option,
            Func<T, Option<TResult>> selector)
        {
            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return OptionModule.IsSome(option)
                ? selector(option.Value)
                : Option<TResult>.None;
        }

        public static Option<T> Where<T>(this Option<T> option, Func<T, bool> predicate)
        {
            if (predicate == null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return option.IsSome && predicate(option.Value)
                ? option
                : Option<T>.None;
        }

        public static Option<T> Where<T>(this FSharpOption<T> option, Func<T, bool> predicate)
        {
            if (predicate == null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return OptionModule.IsSome(option) && predicate(option.Value)
                ? (Option<T>) option
                : Option<T>.None;
        }

        public static TAccumulate Aggregate<T, TAccumulate>(
            this Option<T> option,
            TAccumulate seed,
            Func<TAccumulate, T, TAccumulate> folder)
        {
            if (seed == null)
            {
                throw new ArgumentNullException(nameof(seed));
            }

            if (folder == null)
            {
                throw new ArgumentNullException(nameof(folder));
            }

            return option.IsSome
                ? folder(seed, option.Value)
                : seed;
        }

        public static TAccumulate Aggregate<T, TAccumulate>(
            this FSharpOption<T> option,
            TAccumulate seed,
            Func<TAccumulate, T, TAccumulate> folder)
        {
            if (seed == null)
            {
                throw new ArgumentNullException(nameof(seed));
            }

            if (folder == null)
            {
                throw new ArgumentNullException(nameof(folder));
            }

            return OptionModule.IsSome(option)
                ? folder(seed, option.Value)
                : seed;
        }

        public static bool TryGetValue<T>(this Option<T> option, out T value)
        {
            if (option.IsSome)
            {
                value = option.Value;
                return true;
            }
            else
            {
                value = default(T);
                return false;
            }
        }

        public static bool TryGetValue<T>(this FSharpOption<T> option, out T value)
        {
            if (OptionModule.IsSome(option))
            {
                value = option.Value;
                return true;
            }
            else
            {
                value = default(T);
                return false;
            }
        }

        public static T GetOrElse<T>(this Option<T> option, Func<T> otherwise)
        {
            if (otherwise == null)
            {
                throw new ArgumentNullException(nameof(otherwise));
            }

            return option.IsSome ? option.Value : otherwise();
        }

        public static T GetOrElse<T>(this FSharpOption<T> option, Func<T> otherwise)
        {
            if (otherwise == null)
            {
                throw new ArgumentNullException(nameof(otherwise));
            }

            return OptionModule.IsSome(option) ? option.Value : otherwise();
        }

        public static FSharpResult<TOk, TError> ToResult<TOk, TError>(
            this Option<TOk> option,
            Func<TError> createError)
        {
            if (createError == null)
            {
                throw new ArgumentNullException(nameof(createError));
            }

            return option.IsSome
                ? FSharpResult<TOk, TError>.NewOk(option.Value)
                : FSharpResult<TOk, TError>.NewError(createError());
        }

        public static FSharpResult<TOk, TError> ToResult<TOk, TError>(
            this FSharpOption<TOk> option,
            Func<TError> createError)
        {
            if (createError == null)
            {
                throw new ArgumentNullException(nameof(createError));
            }

            return OptionModule.IsSome(option)
                ? FSharpResult<TOk, TError>.NewOk(option.Value)
                : FSharpResult<TOk, TError>.NewError(createError());
        }
    }

    public static class EnumerableOptionExtensions
    {
        public static Option<T> FirstOrNone<T>(this IEnumerable<T> sequence) where T : class
        {
            if (sequence == null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            return OptionModule.OfObj(sequence.FirstOrDefault());
        }

        public static Option<T> FirstOrNone<T>(this IEnumerable<T> sequence, Func<T, bool> predicate) where T : class
        {
            if (sequence == null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            if (predicate == null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return OptionModule.OfObj(sequence.FirstOrDefault(predicate));
        }

        public static Option<T> SingleOrNone<T>(this IEnumerable<T> sequence) where T : class
        {
            if (sequence == null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            return OptionModule.OfObj(sequence.SingleOrDefault());
        }

        public static Option<T> SingleOrNone<T>(this IEnumerable<T> sequence, Func<T, bool> predicate) where T : class
        {
            if (sequence == null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            if (predicate == null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return OptionModule.OfObj(sequence.SingleOrDefault(predicate));
        }
    }
}