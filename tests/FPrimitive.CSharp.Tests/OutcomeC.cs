using System;
using System.Collections.Generic;
using System.Linq;

namespace FPrimitive.CSharp.Tests
{
    /// <summary>
    /// Represents an abstracted result that can either represent a successful or failure outcome.
    /// </summary>
    /// <typeparam name="T">The type of the successful value.</typeparam>
    /// <typeparam name="TError">The type of the failure error.</typeparam>
    public struct OutcomeC<T, TError> : IEquatable<OutcomeC<T, TError>>
    {
        private readonly T _value;
        private readonly TError _error;

        /// <summary>
        /// Initializes a new instance of the <see cref="Outcome"/> class.
        /// </summary>
        /// <param name="value">The successful value of this outcome.</param>
        public OutcomeC(T value)
        {
            if (Object.ReferenceEquals(value, null))
            {
                throw new ArgumentNullException(nameof(value));
            }

            _value = value;
            _error = default(TError);

            IsSuccess = true;
            IsFailure = false;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Outcome"/> class.
        /// </summary>
        /// <param name="error">The failure error of this outcome.</param>
        public OutcomeC(TError error)
        {
            if (Object.ReferenceEquals(error, null))
            {
                throw new ArgumentNullException(nameof(error));
            }

            _value = default(T);
            _error = error;

            IsSuccess = false;
            IsFailure = true;
        }

        /// <summary>
        /// Gets a flag indicating this result instance represents a successful outcome.
        /// </summary>
        public bool IsSuccess { get; }

        /// <summary>
        /// Gets a flag indicating this result instance represents a failure outcome.
        /// </summary>
        public bool IsFailure { get; }

        /// <summary>
        /// Gets the successful value of this outcome;
        /// throw a <see cref="NotSuccessfulException"/> when this outcome represents a failure outcome.
        /// </summary>
        public T Value => IsSuccess ? _value : throw new NotSuccessfulException($"Cannot get successful value of Outcome<{typeof(T).Name}, {typeof(TError).Name}> because it represents a failure outcome");

        /// <summary>
        /// Gets the failure error of this outcome;
        /// throw a <see cref="NotFailureException"/> when this outcome represents a successful outcome.
        /// </summary>
        public TError Error => IsFailure ? _error : throw new NotFailureException($"Cannot get failure error of Outcome<{typeof(T).Name}, {typeof(TError).Name}> because it represents a successful outcome");

        /// <summary>
        /// Indicates whether this instance and a specified object are equal.
        /// </summary>
        /// <param name="obj">The object to compare with the current instance.</param>
        /// <returns>true if <paramref name="obj">obj</paramref> and this instance are the same type and represent the same value; otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            return obj is OutcomeC<T, TError> other && Equals(other);
        }

        /// <summary>
        /// Indicates whether the current object is equal to another object of the same type.
        /// </summary>
        /// <param name="other">An object to compare with this object.</param>
        /// <returns>true if the current object is equal to the <paramref name="other">other</paramref> parameter; otherwise, false.</returns>
        public bool Equals(OutcomeC<T, TError> other)
        {
            if (IsSuccess && other.IsFailure || IsFailure && other.IsSuccess)
            {
                return false;
            }

            if (IsSuccess && other.IsSuccess)
            {
                return EqualityComparer<T>.Default.Equals(_value, other._value);
            }

            if (IsFailure && other.IsFailure)
            {
                return EqualityComparer<TError>.Default.Equals(_error, other._error);
            }

            return false;
        }

        /// <summary>
        /// Indicates whether the current object is equal to another object of the same type.
        /// </summary>
        /// <param name="other">An object to compare with this object.</param>
        /// <param name="successfulComparer">The instance to compare successful outcomes.</param>
        /// <param name="failureComparer">The instance to compare failure outcomes.</param>
        /// <returns>true if the current object is equal to the <paramref name="other">other</paramref> parameter; otherwise, false.</returns>
        public bool Equals(
            OutcomeC<T, TError> other, 
            IEqualityComparer<T> successfulComparer, 
            IEqualityComparer<TError> failureComparer)
        {
            if (successfulComparer is null)
            {
                throw new ArgumentNullException(nameof(successfulComparer));
            }

            if (failureComparer is null)
            {
                throw new ArgumentNullException(nameof(failureComparer));
            }

            if (IsSuccess && other.IsFailure || IsFailure && other.IsSuccess)
            {
                return false;
            }

            if (IsSuccess && other.IsSuccess)
            {
                return EqualityComparer<T>.Default.Equals(_value, other._value);
            }

            if (IsFailure && other.IsFailure)
            {
                return EqualityComparer<TError>.Default.Equals(_error, other._error);
            }

            return false;
        }

        /// <summary>
        /// Returns the hash code for this instance.
        /// </summary>
        /// <returns>A 32-bit signed integer that is the hash code for this instance.</returns>
        public override int GetHashCode()
        {
            if (IsSuccess)
            {
                return _value.GetHashCode();
            }
            else if (IsFailure)
            {
                return _error.GetHashCode();
            }
            else
            {
                return 0;
            }
        }

        /// <summary>
        /// Returns a value that indicates whether the values of two <see cref="T:FPrimitive.CSharp.Tests.Outcome`2" /> objects are equal.
        /// </summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if the <paramref name="left" /> and <paramref name="right" /> parameters have the same value; otherwise, false.</returns>
        public static bool operator ==(OutcomeC<T, TError> left, OutcomeC<T, TError> right)
        {
            return left.Equals(right);
        }

        /// <summary>
        /// Returns a value that indicates whether two <see cref="T:FPrimitive.CSharp.Tests.Outcome`2" /> objects have different values.
        /// </summary>
        /// <param name="left">The first value to compare.</param>
        /// <param name="right">The second value to compare.</param>
        /// <returns>true if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
        public static bool operator !=(OutcomeC<T, TError> left, OutcomeC<T, TError> right)
        {
            return !left.Equals(right);
        }

        /// <summary>
        /// Creates a successful outcome with given <paramref name="value"/>.
        /// </summary>
        /// <param name="value">The successful value.</param>
        public static OutcomeC<T, TError> Success(T value)
        {
            return new OutcomeC<T, TError>(value);
        }

        /// <summary>
        /// Creates a failure outcome with a given <paramref name="error"/>.
        /// </summary>
        /// <param name="error">The failure error.</param>
        public static OutcomeC<T, TError> Failure(TError error)
        {
            return new OutcomeC<T, TError>(error);
        }

        public static implicit operator OutcomeC<T, TError>(T value)
        {
            return new OutcomeC<T, TError>(value);
        }

        public static implicit operator OutcomeC<T, TError>(TError error)
        {
            return new OutcomeC<T, TError>(error);
        }
    }

    /// <summary>
    /// Represents an abstracted result that can either represent a successful or failure outcome.
    /// </summary>
    public static class Outcome
    {
        /// <summary>
        /// Creates a successful outcome with given <paramref name="value"/>.
        /// </summary>
        /// <param name="value">The successful value.</param>
        public static OutcomeC<T, TError> Success<T, TError>(T value)
        {
            return new OutcomeC<T, TError>(value);
        }

        /// <summary>
        /// Creates a failure outcome with a given <paramref name="error"/>.
        /// </summary>
        /// <param name="error">The failure error.</param>
        public static OutcomeC<T, TError> Failure<T, TError>(TError error)
        {
            return new OutcomeC<T, TError>(error);
        }

        /// <summary>
        /// Creates an outcome instance representing a <paramref name="value"/> when the given <paramref name="predicate"/> holds.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <param name="value">The value to verify.</param>
        /// <param name="predicate">The predicate that determines if the <paramref name="value"/> is successful.</param>
        /// <param name="error">The error when the <paramref name="predicate"/> doesn't hold.'</param>
        public static OutcomeC<T, TError> Create<T, TError>(T value, Func<T, bool> predicate, TError error)
        {
            if (predicate is null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return predicate(value) 
                ? new OutcomeC<T, TError>(value) 
                : new OutcomeC<T, TError>(error);
        }

        /// <summary>
        /// Creates an outcome instance representing a <paramref name="value"/> when the given <paramref name="predicate"/> holds.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <param name="value">The value to verify.</param>
        /// <param name="predicate">The predicate that determines if the <paramref name="value"/> is successful.</param>
        /// <param name="createError">The creation function to create an error when the <paramref name="predicate"/> doesn't hold.'</param>
        public static OutcomeC<T, TError> Create<T, TError>(T value, Func<T, bool> predicate, Func<TError> createError)
        {
            if (predicate is null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            if (createError is null)
            {
                throw new ArgumentNullException(nameof(createError));
            }

            return predicate(value) 
                ? new OutcomeC<T, TError>(value) 
                : new OutcomeC<T, TError>(createError());
        }

        /// <summary>
        /// Creates an outcome instance representing a <paramref name="value"/> when the given <paramref name="predicate"/> holds.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <param name="value">The value to verify.</param>
        /// <param name="predicate">The predicate that determines if the <paramref name="value"/> is successful.</param>
        public static OutcomeC<T, TError> Create<T, TError>(T value, Func<T, (bool, TError)> predicate)
        {
            if (predicate is null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            (bool isSuccessful, TError error) = predicate(value);
            return isSuccessful
                ? new OutcomeC<T, TError>(value)
                : new OutcomeC<T, TError>(error);
        }

        /// <summary>
        /// Simulates a using statement where the exception gets caught into an outcome.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <typeparam name="TResult">The result type after the resource was used.</typeparam>
        /// <typeparam name="TException">The type of the exception to catch.</typeparam>
        /// <param name="createResource">The creation function to create the disposable resource.</param>
        /// <param name="useResource">The function to run for the resource, gets disposed afterwards.</param>
        /// <param name="handler">The handler when an exception occurs during the <paramref name="useResource"/>.</param>
        public static OutcomeC<TResult, TError> Using<T, TError, TResult, TException>(
            Func<T> createResource,
            Func<T, TResult> useResource,
            Func<TException, TError> handler) 
            where T : IDisposable 
            where TException : Exception
        {
            if (createResource is null)
            {
                throw new ArgumentNullException(nameof(createResource));
            }

            if (useResource is null)
            {
                throw new ArgumentNullException(nameof(useResource));
            }

            if (handler is null)
            {
                throw new ArgumentNullException(nameof(handler));
            }

            try
            {
                using (T resource = createResource())
                {
                    return new OutcomeC<TResult, TError>(useResource(resource));
                }
            }
            catch (TException exception)
            {
                return new OutcomeC<TResult, TError>(handler(exception));
            }
        }

        /// <summary>
        /// Simulates a try...catch statement where an exception gets caught into an outcome.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <typeparam name="TResult">The result type after the function was invoked.</typeparam>
        /// <param name="value">The value as input for the catched <paramref name="critical"/> function.</param>
        /// <param name="critical">The function to be caught.</param>
        /// <param name="handler">The handler to create an <typeparamref name="TError"/> when an exception is thrown.</param>
        public static OutcomeC<TResult, TError> Catch<T, TError, TResult>(
            T value,
            Func<T, TResult> critical,
            Func<Exception, TError> handler)
        {
            if (critical is null)
            {
                throw new ArgumentNullException(nameof(critical));
            }

            if (handler is null)
            {
                throw new ArgumentNullException(nameof(handler));
            }

            return Catch<T, TError, TResult, Exception>(value, critical, handler);
        }

        /// <summary>
        /// Simulates a try...catch statement where an exception gets caught into an outcome.
        /// </summary>
        /// <typeparam name="T">The type of the successful value.</typeparam>
        /// <typeparam name="TError">The type of the failure error.</typeparam>
        /// <typeparam name="TResult">The result type after the function was invoked.</typeparam>
        /// <typeparam name="TException">The type of the exception to catch.</typeparam>
        /// <param name="value">The value as input for the catched <paramref name="critical"/> function.</param>
        /// <param name="critical">The function to be caught.</param>
        /// <param name="handler">The handler to create an <typeparamref name="TError"/> when an exception is thrown.</param>
        public static OutcomeC<TResult, TError> Catch<T, TError, TResult, TException>(
            T value,
            Func<T, TResult> critical,
            Func<TException, TError> handler)
            where TException : Exception
        {
            if (critical is null)
            {
                throw new ArgumentNullException(nameof(critical));
            }

            if (handler is null)
            {
                throw new ArgumentNullException(nameof(handler));
            }

            try
            {
                return new OutcomeC<TResult, TError>(critical(value));
            }
            catch (TException exception)
            {
                return new OutcomeC<TResult, TError>(handler(exception));
            }
        }
    }

    /// <summary>
    /// Exception thrown when the successful value of a result instance is called while the instance represents a failure outcome.
    /// </summary>
    [Serializable]
    public class NotSuccessfulException : Exception
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        public NotSuccessfulException() { }

        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        /// <param name="message">The message that describes the exception.</param>
        public NotSuccessfulException(string message) : base(message) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        /// <param name="message">The message that describes the exception.</param>
        /// <param name="innerException">The exception that is the cause of the current exception.</param>
        public NotSuccessfulException(string message, Exception innerException) : base(message, innerException) { }
    }

    /// <summary>
    /// Exception thrown when the failure value of a result instance is called while the instance represents a successful outcome.
    /// </summary>
    [Serializable]
    public class NotFailureException : Exception
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        public NotFailureException() { }

        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        /// <param name="message">The message that describes the exception.</param>
        public NotFailureException(string message) : base(message) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="NotSuccessfulException"/> class.
        /// </summary>
        /// <param name="message">The message that describes the exception.</param>
        /// <param name="innerException">The exception that is the cause of the current exception.</param>
        public NotFailureException(string message, Exception innerException) : base(message, innerException) { }
    }

    /// <summary>
    /// Adds additional operations on the <see cref="OutcomeC{T,TError}"/> type.
    /// </summary>
    public static class OutcomeExtensions
    {
        /// <summary>
        /// Transforms the inner value of the result instance to another value.
        /// </summary>
        /// <param name="outcome">The instance to transform.</param>
        /// <param name="selector">The function to create another successful representation for the <paramref name="outcome"/>.</param>
        public static OutcomeC<TResult, TError> Select<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, TResult> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return outcome.IsSuccess
                ? OutcomeC<TResult, TError>.Success(selector(outcome.Value))
                : OutcomeC<TResult, TError>.Failure(outcome.Error);
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another value.
        /// </summary>
        /// <param name="result">The instance to transform.</param>
        /// <param name="selector">The function to create another failure representation for the <paramref name="result"/>.</param>
        public static OutcomeC<T, TErrorResult> Select<T, TError, TErrorResult>(
            this OutcomeC<T, TError> result,
            Func<TError, TErrorResult> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return result.IsFailure
                ? OutcomeC<T, TErrorResult>.Failure(selector(result.Error))
                : OutcomeC<T, TErrorResult>.Success(result.Value);
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another value.
        /// </summary>
        /// <param name="result">The instance to transform.</param>
        /// <param name="selector">The function to create another successful representation for the <paramref name="result"/>.</param>
        /// <param name="selectorError">The function to create another failure representation for the <paramref name="result"/>.</param>
        public static OutcomeC<TResult, TErrorResult> Select<T, TError, TResult, TErrorResult>(
            this OutcomeC<T, TError> result,
            Func<T, TResult> selector,
            Func<TError, TErrorResult> selectorError)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            if (selectorError is null)
            {
                throw new ArgumentNullException(nameof(selectorError));
            }

            return result.IsSuccess
                ? OutcomeC<TResult, TErrorResult>.Success(selector(result.Value))
                : OutcomeC<TResult, TErrorResult>.Failure(selectorError(result.Error));
        }

        /// <summary>
        /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler.
        /// </summary>
        /// <param name="result">The instance to run throughout a try/catch function.</param>
        /// <param name="selector">The function that will use the successful outcome value.</param>
        /// <param name="handler">The exception handler function that will transform the exception to an failure outcome.</param>
        public static OutcomeC<TResult, TError> Catch<T, TError, TResult>(
            this OutcomeC<T, TError> result,
            Func<T, TResult> selector,
            Func<Exception, TError> handler)
        {
            return result.Catch<T, TError, TResult, Exception>(selector, handler);
        }

        /// <summary>
        /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler.
        /// </summary>
        /// <param name="result">The instance to run throughout a try/catch function.</param>
        /// <param name="selector">The function that will use the successful outcome value.</param>
        /// <param name="handler">The exception handler function that will transform the exception to an failure outcome.</param>
        public static OutcomeC<TResult, TError> Catch<T, TError, TResult, TException>(
            this OutcomeC<T, TError> result,
            Func<T, TResult> selector,
            Func<TException, TError> handler) 
            where TException : Exception
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            if (handler is null)
            {
                throw new ArgumentNullException(nameof(handler));
            }

            try
            {
                return result.Select(selector);
            }
            catch (TException exception)
            {
                return handler(exception);
            }
        }

        /// <summary>
        /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler and disposes the resource afterwards.
        /// </summary>
        /// <param name="result">The instance to run throughout a using function.</param>
        /// <param name="selector">The function that will use the successful outcome value.</param>
        /// <param name="handler">The exception handler function that will transform the exception to an failure outcome.</param>
        public static OutcomeC<TResult, TError> Using<T, TError, TResult>(
            this OutcomeC<T, TError> result,
            Func<T, TResult> selector,
            Func<Exception, TError> handler)
            where T : IDisposable
        {
            return result.Using<T, TError, TResult, Exception>(selector, handler);
        }

        /// <summary>
        /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler and disposes the resource afterwards.
        /// </summary>
        /// <param name="result">The instance to run throughout a using function.</param>
        /// <param name="selector">The function that will use the successful outcome value.</param>
        /// <param name="handler">The exception handler function that will transform the exception to an failure outcome.</param>
        public static OutcomeC<TResult, TError> Using<T, TError, TResult, TException>(
            this OutcomeC<T, TError> result,
            Func<T, TResult> selector,
            Func<TException, TError> handler)
            where T : IDisposable
            where TException : Exception
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            if (handler is null)
            {
                throw new ArgumentNullException(nameof(handler));
            }

            if (result.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(result.Error);
            }

            try
            {
                using (T resource = result.Value)
                {
                    return OutcomeC<TResult, TError>.Success(selector(resource));
                }
            }
            catch (TException exception)
            {
                return OutcomeC<TResult, TError>.Failure(handler(exception));
            }
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another result instance, flatting the result. 
        /// </summary>
        /// <param name="outcome">The instance to transform.</param>
        /// <param name="selector">The function to create another representation for the <paramref name="outcome"/>.</param>
        public static OutcomeC<TResult, TError> SelectMany<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, OutcomeC<TResult, TError>> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(outcome));
            }

            return outcome.IsSuccess
                ? selector(outcome.Value)
                : new OutcomeC<TResult, TError>(outcome.Error);
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another value.
        /// </summary>
        /// <param name="outcome">The instance to transform.</param>
        /// <param name="selector">The function to create another successful value.</param>
        public static OutcomeC<TResult, TError> SelectMany<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, TResult> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(outcome));
            }

            return outcome.IsSuccess
                ? new OutcomeC<TResult, TError>(selector(outcome.Value))
                : new OutcomeC<TResult, TError>(outcome.Error);
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another result instance, flatting the result. 
        /// </summary>
        /// <param name="outcome">The instance to transform.</param>
        /// <param name="selector">The function to create another representation for the <paramref name="outcome"/>.</param>
        public static OutcomeC<TResult, TError> Then<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, OutcomeC<TResult, TError>> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(outcome));
            }

            return outcome.IsSuccess
                ? selector(outcome.Value)
                : new OutcomeC<TResult, TError>(outcome.Error);
        }

        /// <summary>
        /// Transforms the inner value of the result instance to another value.
        /// </summary>
        /// <param name="outcome">The instance to transform.</param>
        /// <param name="selector">The function to create another successful value.</param>
        public static OutcomeC<TResult, TError> Then<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, TResult> selector)
        {
            if (selector is null)
            {
                throw new ArgumentNullException(nameof(outcome));
            }

            return outcome.IsSuccess
                ? new OutcomeC<TResult, TError>(selector(outcome.Value))
                : new OutcomeC<TResult, TError>(outcome.Error);
        }

        /// <summary>
        /// Filters an optional instance by the given predicate, returning a failure outcome result when the predicate doesn't hold.
        /// </summary>
        /// <param name="outcome">The instance to filter.</param>
        /// <param name="predicate">The predicate to filter the successful value.</param>
        /// <param name="error">The failure value when the <paramref name="predicate"/> doesn't hold.</param>
        public static OutcomeC<T, TError> Where<T, TError>(
            this OutcomeC<T, TError> outcome,
            Func<T, bool> predicate,
            TError error)
        {
            if (predicate is null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            return outcome.IsSuccess && predicate(outcome.Value) 
                ? outcome 
                : OutcomeC<T, TError>.Failure(error);
        }

        /// <summary>
        /// Filters an optional instance by the given predicate, returning a failure outcome result when the predicate doesn't hold.
        /// </summary>
        /// <param name="outcome">The instance to filter.</param>
        /// <param name="predicate">The predicate to filter the successful value.</param>
        /// <param name="createError">The creation function when the <paramref name="predicate"/> doesn't hold.</param>
        public static OutcomeC<T, TError> Where<T, TError>(
            this OutcomeC<T, TError> outcome,
            Func<T, bool> predicate,
            Func<TError> createError)
        {
            if (predicate is null)
            {
                throw new ArgumentNullException(nameof(predicate));
            }

            if (createError is null)
            {
                throw new ArgumentNullException(nameof(createError));
            }

            return outcome.IsSuccess && predicate(outcome.Value)
                ? outcome
                : OutcomeC<T, TError>.Failure(createError());
        }

        /// <summary>
        /// Collapse an result instance to another type from the seed via the given aggregator function.
        /// </summary>
        /// <param name="outcome">The instance to aggregate.</param>
        /// <param name="seed">The initial value during aggregation and fallback when failure.</param>
        /// <param name="aggregator">The function to aggregate the successful value into something else.</param>
        public static OutcomeC<TResult, TError> Aggregate<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            TResult seed,
            Func<TResult, T, TResult> aggregator)
        {
            if (aggregator is null)
            {
                throw new ArgumentNullException(nameof(aggregator));
            }

            return outcome.IsSuccess ? aggregator(seed, outcome.Value) : seed;
        }

        /// <summary>
        /// Runs an action on the inner value, without any result. Useful for logging.
        /// </summary>
        /// <param name="outcome">The instance when running the action.</param>
        /// <param name="action">The function to run without any result.</param>
        public static OutcomeC<T, TError> Do<T, TError>(
            this OutcomeC<T, TError> outcome,
            Action<T> action)
        {
            if (action is null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            if (outcome.IsSuccess)
            {
                action(outcome.Value);
            }

            return outcome;
        }

        /// <summary>
        /// Runs an action on the inner value, without any result. Useful for logging.
        /// </summary>
        /// <param name="outcome">The instance when running the action.</param>
        /// <param name="action">The function to run without any result.</param>
        public static OutcomeC<T, TError> Do<T, TError>(
            this OutcomeC<T, TError> outcome,
            Action<TError> action)
        {
            if (action is null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            if (outcome.IsFailure)
            {
                action(outcome.Error);
            }

            return outcome;
        }

        /// <summary>
        /// Runs an action on the inner value, without any result. Useful for logging.
        /// </summary>
        /// <param name="outcome">The instance when running the action.</param>
        /// <param name="action">The function to run on the successful value without any result.</param>
        /// <param name="actionError">The function on the failure value without any result.</param>
        public static OutcomeC<T, TError> Do<T, TError>(
            this OutcomeC<T, TError> outcome,
            Action<T> action,
            Action<TError> actionError)
        {
            if (action is null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            if (outcome.IsSuccess)
            {
                action(outcome.Value);
            }

            if (outcome.IsFailure)
            {
                actionError(outcome.Error);
            }

            return outcome;
        }

        /// <summary>
        /// Combine two result instances with a set of given zipper functions.
        /// </summary>
        /// <param name="outcome1">The first outcome to combine.</param>
        /// <param name="outcome2">The second outcome to combine.</param>
        /// <param name="zipper">The function to combine successful values.</param>
        /// <param name="zipperError">The function to determine the failure value.</param>
        public static OutcomeC<TResult, TError> Zip<T1, T2, TError, TResult>(
            this OutcomeC<T1, TError> outcome1,
            OutcomeC<T2, TError> outcome2,
            Func<T1, T2, TResult> zipper,
            Func<TError, TError, TError> zipperError)
        {
            if (zipper is null)
            {
                throw new ArgumentNullException(nameof(zipper));
            }

            if (zipperError is null)
            {
                throw new ArgumentNullException(nameof(zipperError));
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Success(zipper(outcome1.Value, outcome2.Value));
            }
            if (outcome1.IsFailure && outcome2.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome2.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome1.Error);
            }

            return OutcomeC<TResult, TError>.Failure(outcome2.Error);
        }

        /// <summary>
        /// Combine three result instances with a set of given zipper functions.
        /// </summary>
        /// <param name="outcome1">The first outcome to combine.</param>
        /// <param name="outcome2">The second outcome to combine.</param>
        /// <param name="outcome3">The third outcome to combine.</param>
        /// <param name="zipper">The function to combine successful values.</param>
        /// <param name="zipperError">The function to determine the failure value.</param>
        public static OutcomeC<TResult, TError> Zip<T1, T2, T3, TError, TResult>(
            this OutcomeC<T1, TError> outcome1,
            OutcomeC<T2, TError> outcome2,
            OutcomeC<T3, TError> outcome3,
            Func<T1, T2, T3, TResult> zipper,
            Func<TError, TError, TError> zipperError)
        {
            if (zipper is null)
            {
                throw new ArgumentNullException(nameof(zipper));
            }

            if (zipperError is null)
            {
                throw new ArgumentNullException(nameof(zipperError));
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Success(zipper(outcome1.Value, outcome2.Value, outcome3.Value));
            }
            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(outcome1.Error, outcome2.Error), outcome3.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess && outcome3.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome1.Error);
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome2.Error);
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(outcome3.Error);
            }

            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome2.Error));
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome2.Error, outcome3.Error));
            }

            return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome3.Error));
        }

        /// <summary>
        /// Combine four result instances with a set of given zipper functions.
        /// </summary>
        /// <param name="outcome1">The first outcome to combine.</param>
        /// <param name="outcome2">The second outcome to combine.</param>
        /// <param name="outcome3">The third outcome to combine.</param>
        /// <param name="outcome4">The four outcome to combine.</param>
        /// <param name="zipper">The function to combine successful values.</param>
        /// <param name="zipperError">The function to determine the failure value.</param>
        public static OutcomeC<TResult, TError> Zip<T1, T2, T3, T4, TError, TResult>(
            this OutcomeC<T1, TError> outcome1,
            OutcomeC<T2, TError> outcome2,
            OutcomeC<T3, TError> outcome3,
            OutcomeC<T4, TError> outcome4,
            Func<T1, T2, T3, T4, TResult> zipper,
            Func<TError, TError, TError> zipperError)
        {
            if (zipper is null)
            {
                throw new ArgumentNullException(nameof(zipper));
            }

            if (zipperError is null)
            {
                throw new ArgumentNullException(nameof(zipperError));
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsSuccess && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Success(zipper(outcome1.Value, outcome2.Value, outcome3.Value, outcome4.Value));
            }

            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsFailure && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(zipperError(outcome1.Error, outcome2.Error), outcome3.Error), outcome4.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess && outcome3.IsSuccess && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome1.Error);
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsSuccess && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome2.Error);
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsFailure && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(outcome3.Error);
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsSuccess && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(outcome4.Error);
            }

            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsSuccess && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome2.Error));
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsFailure && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome2.Error, outcome3.Error));
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess && outcome3.IsFailure && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome3.Error, outcome4.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess && outcome3.IsFailure && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome3.Error));
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsSuccess && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(outcome2.Error, outcome4.Error));
            }

            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsFailure && outcome4.IsSuccess)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(outcome1.Error, outcome2.Error), outcome3.Error));
            }

            if (outcome1.IsSuccess && outcome2.IsFailure && outcome3.IsFailure && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(outcome2.Error, outcome3.Error), outcome4.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess && outcome3.IsFailure && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(outcome1.Error, outcome3.Error), outcome4.Error));
            }

            if (outcome1.IsFailure && outcome2.IsFailure && outcome3.IsSuccess && outcome4.IsFailure)
            {
                return OutcomeC<TResult, TError>.Failure(zipperError(zipperError(outcome1.Error, outcome2.Error), outcome4.Error));
            }

            return OutcomeC<TResult, TError>.Failure(zipperError(outcome1.Error, outcome4.Error));
        }

        /// <summary>
        /// Transforms two results together, the abstracted 'plus' (+) operator.
        /// </summary>
        /// <param name="outcome1">The first outcome to 'plus' together.</param>
        /// <param name="outcome2">The second outcome to 'plus' together.</param>
        /// <param name="ifSuccess">The function when both results are successful.</param>
        /// <param name="ifFailure">The function when both results are failure.</param>
        public static OutcomeC<T, TError> Plus<T, TError>(
            this OutcomeC<T, TError> outcome1,
            OutcomeC<T, TError> outcome2,
            Func<T, T, T> ifSuccess,
            Func<TError, TError, TError> ifFailure)
        {
            if (ifSuccess is null)
            {
                throw new ArgumentNullException(nameof(ifSuccess));
            }

            if (ifFailure is null)
            {
                throw new ArgumentNullException(nameof(ifFailure));
            }

            if (outcome1.IsSuccess && outcome2.IsSuccess)
            {
                return OutcomeC<T, TError>.Success(ifSuccess(outcome1.Value, outcome2.Value));
            }

            if (outcome1.IsFailure && outcome2.IsFailure)
            {
                return OutcomeC<T, TError>.Failure(ifFailure(outcome1.Error, outcome2.Error));
            }

            if (outcome1.IsFailure && outcome2.IsSuccess)
            {
                return outcome1;
            }

            return outcome2;
        }

        /// <summary>
        ///  Switch to another result if the current result represents a failure.
        /// </summary>
        /// <param name="outcome">The outcome to switch.</param>
        /// <param name="otherwise">The function to create another outcome.</param>
        public static OutcomeC<T, TError> OrElse<T, TError>(
            this OutcomeC<T, TError> outcome,
            Func<TError, OutcomeC<T, TError>> otherwise)
        {
            if (otherwise is null)
            {
                throw new ArgumentNullException(nameof(otherwise));
            }

            return outcome.IsSuccess ? outcome : otherwise(outcome.Error);
        }

        /// <summary>
        /// Gets the successful value of the outcome instance, or an evaluated default value.
        /// </summary>
        /// <param name="outcome">The outcome to get the successful value from.</param>
        /// <param name="otherwise">The function to evaluate the alternative when the outcome represents a failure.</param>
        public static T GetOrElse<T, TError>(
            this OutcomeC<T, TError> outcome,
            Func<T> otherwise)
        {
            if (otherwise is null)
            {
                throw new ArgumentNullException(nameof(otherwise));
            }

            return outcome.IsSuccess ? outcome.Value : otherwise();
        }

        /// <summary>
        /// Gets the successful value of the outcome instance, or an evaluated default value.
        /// </summary>
        /// <param name="outcome">The outcome to get the successful value from.</param>
        /// <param name="otherwise">The function to evaluate the alternative when the outcome represents a failure.</param>
        public static T GetOrElse<T, TError>(
            this OutcomeC<T, TError> outcome,
            T otherwise)
        {
            return outcome.IsSuccess ? outcome.Value : otherwise;
        }

        /// <summary>
        /// Gets the successful value of the outcome instance, or an evaluated default value.
        /// </summary>
        /// <param name="outcome">The outcome to get the successful value from.</param>
        /// <param name="otherwise">The function to evaluate the alternative when the outcome represents a success.</param>
        public static TError GetOrElse<T, TError>(this OutcomeC<T, TError> outcome, Func<TError> otherwise)
        {
            if (otherwise is null)
            {
                throw new ArgumentNullException(nameof(otherwise));
            }

            return outcome.IsFailure ? outcome.Error : otherwise();
        }
        
        /// <summary>
        /// Gets the successful value of the outcome instance, or an evaluated default value.
        /// </summary>
        /// <param name="outcome">The outcome to get the successful value from.</param>
        /// <param name="otherwise">The function to evaluate the alternative when the outcome represents a success.</param>
        public static TError GetOrElse<T, TError>(this OutcomeC<T, TError> outcome, TError otherwise)
        {
            return outcome.IsFailure ? outcome.Error : otherwise;
        }

        /// <summary>
        /// Tries to get the successful value out of the result.
        /// </summary>
        /// <param name="outcome">The outcome to get the successful value from.</param>
        /// <param name="result">The output where the successful value will be set.</param>
        public static bool TryGet<T, TError>(this OutcomeC<T, TError> outcome, out T result)
        {
            if (outcome.IsSuccess)
            {
                result = outcome.Value;
                return true;
            }

            result = default(T);
            return false;
        }

        /// <summary>
        /// Tries to get the failure value out of the result.
        /// </summary>
        /// <param name="outcome">The outcome to get the failure value from.</param>
        /// <param name="result">The output where the failure value will be set.</param>
        public static bool TryGetError<T, TError>(this OutcomeC<T, TError> outcome, out TError result)
        {
            if (outcome.IsFailure)
            {
                result = outcome.Error;
                return true;
            }

            result = default(TError);
            return false;
        }

        /// <summary>
        /// Transforms both the successful and failure value of the outcome instance to another type.
        /// </summary>
        /// <param name="outcome">The outcome to transform.</param>
        /// <param name="ifSuccess">The function to transform the successful value.</param>
        /// <param name="ifFailure">The function to transform the failure value.</param>
        public static TResult Match<T, TError, TResult>(
            this OutcomeC<T, TError> outcome,
            Func<T, TResult> ifSuccess,
            Func<TError, TResult> ifFailure)
        {
            if (ifSuccess is null)
            {
                throw new ArgumentNullException(nameof(ifSuccess));
            }

            if (ifFailure is null)
            {
                throw new ArgumentNullException(nameof(ifFailure));
            }

            return outcome.IsSuccess ? ifSuccess(outcome.Value) : ifFailure(outcome.Error);
        }

        /// <summary>
        /// Traverse the sequence, running a mapping function over the elements, collecting the results into an outcome instance.
        /// </summary>
        /// <param name="sequence">The sequence of elements to each traverse over a outcome function.</param>
        /// <param name="selector">The outcome function which each element should run through.</param>
        public static OutcomeC<IEnumerable<TResult>, TError> Traverse<T, TError, TResult>(
            this IEnumerable<T> sequence,
            Func<T, OutcomeC<TResult, TError>> selector)
        {
            if (sequence is null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            if (selector is null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            return sequence.Aggregate(
                OutcomeC<IEnumerable<TResult>, TError>.Success(Enumerable.Empty<TResult>()),
                (acc, x) => acc.SelectMany(xs => selector(x).Select(xs.Append)));
        }

        /// <summary>
        /// Transforms a sequence of outcome instances to a single outcome instance with a sequence.
        /// </summary>
        /// <param name="sequence">The sequence of outcome instances to transform.</param>
        public static OutcomeC<IEnumerable<T>, TError> Sequence<T, TError>(
            this IEnumerable<OutcomeC<T, TError>> sequence)
        {
            if (sequence is null)
            {
                throw new ArgumentNullException(nameof(sequence));
            }

            return Traverse(sequence, x => x);
        }

        /// <summary>
        /// Use multiple error types for the outcome instance.
        /// </summary>
        /// <param name="outcome">The instance to have many failures.</param>
        public static OutcomeC<T, IEnumerable<TError>> ManyErrors<T, TError>(
            this OutcomeC<T, TError> outcome)
        {
            return outcome.Select((TError err) => Enumerable.Repeat(err, 1));
        }

        /// <summary>
        /// Append error to the outcome instance.
        /// </summary>
        /// <param name="outcome">The instance to append failures to.</param>
        /// <param name="error">The failure to add.</param>
        public static OutcomeC<T, IEnumerable<TError>> AppendError<T, TError>(
            this OutcomeC<T, IEnumerable<TError>> outcome,
            TError error)
        {
            return outcome.Select(err => err.Append(error));
        }

        /// <summary>
        /// Append errors to the outcome instance.
        /// </summary>
        /// <param name="outcome">The instance to append failures to.</param>
        /// <param name="errors">The failures to add.</param>
        public static OutcomeC<T, IEnumerable<TError>> AppendErrors<T, TError>(
            this OutcomeC<T, IEnumerable<TError>> outcome,
            IEnumerable<TError> errors)
        {
            if (errors is null)
            {
                throw new ArgumentNullException(nameof(errors));
            }

            return outcome.Select(err => err.Concat(errors));
        }

        /// <summary>
        /// Format errors in the outcome instance.
        /// </summary>
        /// <param name="outcome">The instance to append failures to.</param>
        /// <param name="separator">The separator text value between failures.</param>
        public static string FormatErrors<T, TError>(
            this OutcomeC<T, IEnumerable<TError>> outcome,
            string separator)
        {
            if (separator is null)
            {
                throw new ArgumentNullException(nameof(separator));
            }

            return outcome.Select((IEnumerable<TError> errors) => String.Join(separator, errors))
                          .GetOrElse(String.Empty);
        }

        /// <summary>
        /// Transforms the outcome instance to a value tuple.
        /// </summary>
        /// <param name="outcome">The instance to deconstruct.</param>
        public static (T, TError) Deconstruct<T, TError>(this OutcomeC<T, TError> outcome)
        {
            return (
                outcome.GetOrElse(default(T)),
                outcome.GetOrElse(default(TError)));
        }
    }
}
