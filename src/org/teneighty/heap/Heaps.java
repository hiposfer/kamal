/*
 * $Id: Heaps.java 14 2009-11-21 18:24:37Z syenkoc $
 * 
 * Copyright (c) 2005-2009 Fran Lattanzio
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package org.teneighty.heap;

import java.io.*;
import java.util.*;

/**
 * This class contains various static methods which operate on heaps. The
 * methods are all polymorphic and will operate on any heap implementation.
 * Several of the methods return decorators around the specified heap.
 * <p>
 * Unless otherwise noted, all methods throw a <code>NullPointerException</code>
 * if the target heap is <code>null</code>.
 * <p>
 * This is a stateless class that cannot be instantiated.
 * 
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 * @see org.teneighty.heap.Heap
 */
public final class Heaps
	extends Object
{

	/**
	 * Create and return a threadsafe view around the specified heap.
	 * <p>
	 * In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing heap is accomplished through
	 * the returned heap.
	 * <p>
	 * The decorated/returned heap uses itself as a mutex; it acts much the
	 * corresponding method in <code>java.util.Collections</code> in this
	 * respect.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @param heap the heap to wrap in a threadsafe view.
	 * @return a synchronized view of the specified heap.
	 * @throws NullPointerException if <code>heap</code> is <code>null</code>.
	 */
	public static <TKey, TValue> Heap<TKey, TValue> synchronizedHeap(
			final Heap<TKey, TValue> heap)
		throws NullPointerException
	{
		if (heap == null)
		{
			throw new NullPointerException();
		}

		return new SynchronizedHeap<TKey, TValue>(heap);
	}

	/**
	 * Create and return an unmodifiable heap view , backed by the specified
	 * heap.
	 * <p>
	 * In order to guarantee unmodifiability, it is critical that
	 * <strong>all</strong> access to the backing heap is accomplished through
	 * the returned heap.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @param heap the heap to make unmodifiable.
	 * @return an unmodifiable view of the specified heap.
	 * @throws NullPointerException if <code>heap</code> is <code>null</code>.
	 */
	public static <TKey, TValue> Heap<TKey, TValue> unmodifiableHeap(
			final Heap<TKey, TValue> heap)
		throws NullPointerException
	{
		if (heap == null)
		{
			throw new NullPointerException();
		}

		return new UnmodifiableHeap<TKey, TValue>(heap);
	}

	/**
	 * Return an empty heap.
	 * <p>
	 * The returned heap is not modifiable.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @return an empty, unmodifiable heap.
	 */
	public static <TKey, TValue> Heap<TKey, TValue> emptyHeap()
	{
		return new EmptyHeap<TKey, TValue>();
	}

	/**
	 * Check for reference equality.
	 * <p>
	 * Lame hack for getting around the fact that the Java compiler will not let
	 * me reference-compare a <code>{@literal Heap<K,V>}</code> and a
	 * <code>{@literal Heap<? extends K, ? extends V>}</code>.
	 * 
	 * @param one the first object.
	 * @param two the second object.
	 * @return true if reference equal; false otherwise.
	 */
	static boolean ReferenceEquals(final Object one, final Object two)
	{
		return (one == two);
	}

	/**
	 * Empty heap class.
	 * <p>
	 * Instances of this class are not modifiable.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class EmptyHeap<TKey, TValue>
		extends Object
		implements Heap<TKey, TValue>, Iterable<Heap.Entry<TKey, TValue>>,
		Serializable
	{

		/**
		 * Serialization nonsense.
		 */
		private static final long serialVersionUID = 476893L;

		/**
		 * Constructor.
		 */
		EmptyHeap()
		{
			super();
		}

		/**
		 * Get the comparator used for decision in this heap.
		 * <p>
		 * If this method returns <code>null</code> then this heap uses the
		 * keys' <i>natural ordering</i>.
		 * 
		 * @return the comparator or <code>null</code>.
		 * @see java.util.Comparator
		 * @see java.lang.Comparable
		 */
		public Comparator<? super TKey> getComparator()
		{
			return null;
		}

		/**
		 * Clear this heap.
		 * 
		 * @throws UnsupportedOperationException Always.
		 */
		public void clear()
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Add a mapping to this heap.
		 * 
		 * @param key the node key.
		 * @param value the node value.
		 * @return the entry created.
		 * @throws UnsupportedOperationException Always.
		 */
		public Entry<TKey, TValue> insert(final TKey key, final TValue value)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Insert all the entries of the specified heap into this heap.
		 * <p>
		 * The other heap will not be cleared, and this heap will simply
		 * <i>hold</i> the entries of <code>other</code>, not <i>contain</i>
		 * them.
		 * 
		 * @param other the other heap.
		 * @throws NullPointerException If <code>other</code> is
		 *             <code>null</code>.
		 * @throws ClassCastException If the keys of <code>other</code> are not
		 *             mutally comparable to the keys of this heap.
		 * @throws IllegalArgumentException If you attempt to insert a heap into
		 *             itself.
		 * @see #union(Heap)
		 */
		public void insertAll(final Heap<? extends TKey, ? extends TValue> other)
			throws NullPointerException, ClassCastException,
			IllegalArgumentException
		{
			if (other == null)
			{
				throw new NullPointerException();
			}

			if (ReferenceEquals(other, this))
			{
				throw new IllegalArgumentException();
			}

			throw new UnsupportedOperationException();
		}

		/**
		 * Get the number of entries in this heap.
		 * 
		 * @return the size.
		 */
		public int getSize()
		{
			return 0;
		}

		/**
		 * Is this heap empty?
		 * 
		 * @return true if this heap is empty.
		 * @see #getSize()
		 */
		public boolean isEmpty()
		{
			return true;
		}

		/**
		 * Get the entry with the minimum key.
		 * <p>
		 * This method does <u>not</u> remove the returned entry.
		 * 
		 * @return the entry.
		 * @throws NoSuchElementException If this heap is empty.
		 * @see #extractMinimum()
		 */
		public Entry<TKey, TValue> getMinimum()
			throws NoSuchElementException
		{
			throw new NoSuchElementException();
		}

		/**
		 * Remove and return the entry minimum key.
		 * 
		 * @return the entry.
		 * @throws UnsupportedOperationException Always.
		 * @see #getMinimum()
		 */
		public Entry<TKey, TValue> extractMinimum()
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Decrease the key of the given element.
		 * <p>
		 * Attempting to decrease the key of an entry which is not a member of
		 * this heap may damage both this heap and the heap of which
		 * <code>e</code> is actually a member beyond repair.
		 * 
		 * @param e the entry for which to decrease the key.
		 * @param key the new key.
		 * @throws UnsupportedOperationException Always.
		 */
		public void decreaseKey(final Entry<TKey, TValue> e, final TKey key)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Delete the entry from this heap.
		 * <p>
		 * Attempting to delete an entry which is not a member of this heap may
		 * damage both this heap and the heap of which <code>e</code> is
		 * actually a member beyond repair.
		 * 
		 * @param e the entry to delete.
		 * @throws UnsupportedOperationException Always.
		 */
		public void delete(final Entry<TKey, TValue> e)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Does this heap hold the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap holds the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean holdsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			if (entry == null)
			{
				throw new NullPointerException();
			}

			return false;
		}

		/**
		 * Does this heap contain the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap contains the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean containsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			if (entry == null)
			{
				throw new NullPointerException();
			}

			return false;
		}

		/**
		 * Union this heap with another heap.
		 * 
		 * @param other the other heap.
		 * @throws UnsupportedOperationException Always.
		 */
		public void union(final Heap<TKey, TValue> other)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Check whether the specified object is referentially or semantically
		 * equal
		 * to this object.
		 * 
		 * @param other the other object.
		 * @return <code>true</code> if equal; <code>false</code> otherwise.
		 */
		@SuppressWarnings("unchecked")
		@Override
		public boolean equals(final Object other)
		{
			if (other == null)
			{
				return false;
			}

			if (this == other)
			{
				return true;
			}

			if (Heap.class.isAssignableFrom(other.getClass()))
			{
				Heap that = (Heap) other;
				return (that.getSize() == 0);
			}

			return false;
		}

		/**
		 * Return the hashcode for this object.
		 * 
		 * @return the hashcode.
		 */
		@Override
		public int hashCode()
		{
			return 0;
		}

		/**
		 * Get a string representation of this object.
		 * 
		 * @return a string.
		 */
		@Override
		public String toString()
		{
			return "EmptyHeap(0){}";
		}

		/**
		 * Get the entry collection.
		 * <p>
		 * The order of the entries in the return collection is arbitrary.
		 * 
		 * @return the entries.
		 * @see org.teneighty.heap.Heap.Entry
		 */
		public Collection<Heap.Entry<TKey, TValue>> getEntries()
		{
			return Collections.emptyList();
		}

		/**
		 * Get the collection of values.
		 * <p>
		 * The order of the values in returned Collection is arbitrary.
		 * 
		 * @return the values.
		 */
		public Collection<TValue> getValues()
		{
			return Collections.emptyList();
		}

		/**
		 * Get the collection of keys.
		 * <p>
		 * The order of the keys in returned Collection is arbitrary.
		 * 
		 * @return the keys.
		 */
		public Collection<TKey> getKeys()
		{
			return Collections.emptyList();
		}

		/**
		 * Get an iterator over the entries in this heap.
		 * 
		 * @return an iterator over the entries.
		 */
		public Iterator<Heap.Entry<TKey, TValue>> iterator()
		{
			// we have this dumbness because the Java compiler is even dumber
			// and cannot correctly pass on the generic types to iterator() in
			// the following statement:
			// return ( Collections.emptyList().iterator() );
			Collection<Heap.Entry<TKey, TValue>> emptyList = Collections
					.emptyList();
			return emptyList.iterator();
		}

		/**
		 * Perform the specified action on each element of this heap.
		 * <p>
		 * It's extremely unwise to attempt to modify the heap (e.g. decrease
		 * the keys of all elements by one). Most implementations of this method
		 * are likely to be implemented atop an iterator over the heap, and
		 * thus, if the iterator is fail-fast and detects concurrent
		 * modification, any changes to the heap will cause the iterator to die.
		 * 
		 * @param action the action to perform.
		 * @throws NullPointerException If <code>action</code> is
		 *             <code>null</code>.
		 */
		public void forEach(Action<Heap.Entry<TKey, TValue>> action)
			throws NullPointerException
		{
			if (action == null)
			{
				throw new NullPointerException();
			}

			// do nothing - since this heap is empty, we have no work to do!
		}

	}

	/**
	 * Unmodifiable heap decorator.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class UnmodifiableHeap<TKey, TValue>
		extends Object
		implements Heap<TKey, TValue>, Iterable<Heap.Entry<TKey, TValue>>,
		Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 23408234L;

		/**
		 * The backing heap.
		 */
		private Heap<TKey, TValue> heap;

		/**
		 * Constructor.
		 * 
		 * @param heap the backing heap.
		 */
		UnmodifiableHeap(final Heap<TKey, TValue> heap)
		{
			super();

			// Store heap.
			this.heap = heap;
		}

		/**
		 * Get the comparator used for decision in this heap.
		 * <p>
		 * If this method returns <code>null</code> then this heap uses the
		 * keys' <i>natural ordering</i>.
		 * 
		 * @return the comparator or <code>null</code>.
		 * @see java.util.Comparator
		 * @see java.lang.Comparable
		 */
		public Comparator<? super TKey> getComparator()
		{
			return this.heap.getComparator();
		}

		/**
		 * Clear this heap.
		 * 
		 * @throws UnsupportedOperationException Always.
		 */
		public void clear()
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Add a mapping to this heap.
		 * 
		 * @param key the node key.
		 * @param value the node value.
		 * @return the entry created.
		 * @throws UnsupportedOperationException Always.
		 */
		public Entry<TKey, TValue> insert(final TKey key, final TValue value)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Insert all the entries of the specified heap into this heap.
		 * <p>
		 * The other heap will not be cleared, and this heap will simply
		 * <i>hold</i> the entries of <code>other</code>, not <i>contain</i>
		 * them.
		 * 
		 * @param other the other heap.
		 * @throws NullPointerException If <code>other</code> is
		 *             <code>null</code>.
		 * @throws ClassCastException If the keys of <code>other</code> are not
		 *             mutally comparable to the keys of this heap.
		 * @throws IllegalArgumentException If you attempt to insert a heap into
		 *             itself.
		 * @see #union(Heap)
		 */
		public void insertAll(final Heap<? extends TKey, ? extends TValue> other)
			throws NullPointerException, ClassCastException,
			IllegalArgumentException
		{
			if (other == null)
			{
				throw new NullPointerException();
			}

			if (ReferenceEquals(other, this)
					|| ReferenceEquals(other, this.heap))
			{
				throw new IllegalArgumentException();
			}

			throw new UnsupportedOperationException();
		}

		/**
		 * Get the number of entries in this heap.
		 * 
		 * @return the size.
		 */
		public int getSize()
		{
			return this.heap.getSize();
		}

		/**
		 * Is this heap empty?
		 * 
		 * @return true if this heap is empty.
		 * @see #getSize()
		 */
		public boolean isEmpty()
		{
			return this.heap.isEmpty();
		}

		/**
		 * Get the entry with the minimum key.
		 * <p>
		 * This method does <u>not</u> remove the returned entry.
		 * 
		 * @return the entry.
		 * @throws NoSuchElementException If this heap is empty.
		 * @see #extractMinimum()
		 */
		public Entry<TKey, TValue> getMinimum()
			throws NoSuchElementException
		{
			return this.heap.getMinimum();
		}

		/**
		 * Remove and return the entry minimum key.
		 * 
		 * @return the entry.
		 * @throws UnsupportedOperationException Always.
		 * @see #getMinimum()
		 */
		public Entry<TKey, TValue> extractMinimum()
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Decrease the key of the given element.
		 * <p>
		 * Attempting to decrease the key of an entry which is not a member of
		 * this heap may damage both this heap and the heap of which
		 * <code>e</code> is actually a member beyond repair.
		 * 
		 * @param e the entry for which to decrease the key.
		 * @param key the new key.
		 * @throws UnsupportedOperationException Always.
		 */
		public void decreaseKey(final Entry<TKey, TValue> e, final TKey key)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Delete the entry from this heap.
		 * <p>
		 * Attempting to delete an entry which is not a member of this heap may
		 * damage both this heap and the heap of which <code>e</code> is
		 * actually a member beyond repair.
		 * 
		 * @param e the entry to delete.
		 * @throws UnsupportedOperationException Always.
		 */
		public void delete(final Entry<TKey, TValue> e)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Does this heap hold the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap holds the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean holdsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			return this.heap.holdsEntry(entry);
		}

		/**
		 * Does this heap contain the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap contains the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean containsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			return this.heap.containsEntry(entry);
		}

		/**
		 * Union this heap with another heap.
		 * 
		 * @param other the other heap.
		 * @throws UnsupportedOperationException Always.
		 */
		public void union(final Heap<TKey, TValue> other)
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Check whether the specified object is referentially or semantically
		 * equal
		 * to this object.
		 * 
		 * @param other the other object.
		 * @return <code>true</code> if equal; <code>false</code> otherwise.
		 */
		@Override
		public boolean equals(final Object other)
		{
			if (other == null)
			{
				return false;
			}

			if (this == other)
			{
				return true;
			}

			return this.heap.equals(other);
		}

		/**
		 * Return a hashcode inline with equals for this object.
		 * 
		 * @return int hashcode the hashcode.
		 */
		@Override
		public int hashCode()
		{
			return this.heap.hashCode();
		}

		/**
		 * Get a string representation of this object.
		 * 
		 * @return a string representation.
		 */
		@Override
		public String toString()
		{
			return this.heap.toString();
		}

		/**
		 * Get the collection of entries held by this heap.
		 * 
		 * @return the entry collection.
		 */
		public Collection<Heap.Entry<TKey, TValue>> getEntries()
		{
			return Collections.unmodifiableCollection(this.heap.getEntries());
		}

		/**
		 * Get the collection of values.
		 * <p>
		 * The order of the values in returned Collection is arbitrary.
		 * 
		 * @return the values.
		 */
		public Collection<TValue> getValues()
		{
			return Collections.unmodifiableCollection(this.heap.getValues());
		}

		/**
		 * Get the collection of keys.
		 * <p>
		 * The order of the keys in returned Collection is arbitrary.
		 * 
		 * @return the keys.
		 */
		public Collection<TKey> getKeys()
		{
			return Collections.unmodifiableCollection(this.heap.getKeys());
		}

		/**
		 * Get an iterator over the entries in this heap.
		 * 
		 * @return an iterator over the entries.
		 */
		public Iterator<Heap.Entry<TKey, TValue>> iterator()
		{
			return new ImmutableHeapEntryIterator<TKey, TValue>(this.heap
					.iterator());
		}

		/**
		 * Perform the specified action on each element of this heap.
		 * <p>
		 * It's extremely unwise to attempt to modify the heap (e.g. decrease
		 * the keys of all elements by one). Most implementations of this method
		 * are likely to be implemented atop an iterator over the heap, and
		 * thus, if the iterator is fail-fast and detects concurrent
		 * modification, any changes to the heap will cause the iterator to die.
		 * 
		 * @param action the action to perform.
		 * @throws NullPointerException If <code>action</code> is
		 *             <code>null</code>.
		 */
		public void forEach(final Action<Heap.Entry<TKey, TValue>> action)
			throws NullPointerException
		{
			// straight delegation...
			this.heap.forEach(action);
		}

		/**
		 * An immutable iterator decorator - i.e. an iterator that doesn't
		 * support
		 * the remove method.
		 * 
		 * @param <TKey> the key type.
		 * @param <TValue> the value type.
		 * @author Fran Lattanzio
		 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
		 *          2009) $
		 */
		private static final class ImmutableHeapEntryIterator<TKey, TValue>
			extends Object
			implements Iterator<Heap.Entry<TKey, TValue>>
		{

			/**
			 * The backing iterator.
			 */
			private Iterator<Heap.Entry<TKey, TValue>> backingIterator;

			/**
			 * Constructor.
			 * 
			 * @param iterator the iterator to use to back this iterator.
			 */
			ImmutableHeapEntryIterator(
					final Iterator<Heap.Entry<TKey, TValue>> iterator)
			{
				// store the backing iterator and stuff.
				this.backingIterator = iterator;
			}

			/**
			 * Check if this iterator has a next element.
			 * 
			 * @return <code>true</code> if there's another entry;
			 *         <code>false</code> otherwise.
			 */
			public boolean hasNext()
			{
				return this.backingIterator.hasNext();
			}

			/**
			 * Get the next heap entry in this iterator.
			 * 
			 * @return the next entry in the iterator.
			 * @throws NoSuchElementException If there are no elements left in
			 *             this
			 *             iterator.
			 */
			public Heap.Entry<TKey, TValue> next()
				throws NoSuchElementException
			{
				// grab next thing.
				Heap.Entry<TKey, TValue> next = this.backingIterator.next();

				// wrap in immutable decorator.
				return new ImmutableHeapEntry<TKey, TValue>(next);
			}

			/**
			 * Remove the most recently iterated entry.
			 * 
			 * @throws UnsupportedOperationException Always.
			 */
			public void remove()
				throws UnsupportedOperationException
			{
				throw new UnsupportedOperationException();
			}

		}

		/**
		 * Immutable wrapper around a heap entry.
		 * 
		 * @param <TKey> the key type.
		 * @param <TValue> the value type.
		 * @author Fran Lattanzio
		 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
		 *          2009) $
		 */
		private static final class ImmutableHeapEntry<TKey, TValue>
			extends Object
			implements Heap.Entry<TKey, TValue>, Serializable
		{

			/**
			 * Serial version UID.
			 */
			private static final long serialVersionUID = 45897356L;

			/**
			 * The backing element.
			 */
			private Heap.Entry<TKey, TValue> entry;

			/**
			 * Constructor.
			 * 
			 * @param towrap the entry to wrap.
			 */
			ImmutableHeapEntry(Heap.Entry<TKey, TValue> towrap)
			{
				super();

				// Store entry.
				this.entry = towrap;
			}

			/**
			 * Get the key
			 * 
			 * @return the key.
			 */
			public TKey getKey()
			{
				return this.entry.getKey();
			}

			/**
			 * Get the value.
			 * 
			 * @return the value.
			 */
			public TValue getValue()
			{
				return this.entry.getValue();
			}

			/**
			 * Set the value.
			 * 
			 * @param val the new value.
			 * @return the old value.
			 * @throws UnsupportedOperationException Always.
			 */
			public TValue setValue(final TValue val)
				throws UnsupportedOperationException
			{
				throw new UnsupportedOperationException();
			}

			/**
			 * Get a string representation of this object.
			 * 
			 * @return a rancid string.
			 */
			@Override
			public String toString()
			{
				return this.entry.toString();
			}

			/**
			 * Compare this object to equality to the specified object.
			 * 
			 * @param other the other object.
			 * @return <code>true</code> if equal; <code>false</code> otherwise.
			 */
			@Override
			public boolean equals(final Object other)
			{
				if (other == null)
				{
					return false;
				}

				if (other == this)
				{
					return true;
				}

				return this.entry.equals(other);
			}

			/**
			 * Get the hash code for this object.
			 * 
			 * @return the hashcode.
			 */
			@Override
			public int hashCode()
			{
				return this.entry.hashCode();
			}

		}

	}

	/**
	 * Synchronized heap decorator.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class SynchronizedHeap<TKey, TValue>
		extends Object
		implements Heap<TKey, TValue>, Iterable<Heap.Entry<TKey, TValue>>,
		Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 4798234789234L;

		/**
		 * The backing heap.
		 */
		private Heap<TKey, TValue> heap;

		/**
		 * The locking object.
		 */
		private transient Object mutex;

		/**
		 * Constructor.
		 * 
		 * @param heap the backing heap.
		 * @throws NullPointerException If <code>heap</code> is
		 *             <code>null</code>.
		 */
		SynchronizedHeap(final Heap<TKey, TValue> heap)
				throws NullPointerException
		{
			super();

			if (heap == null)
			{
				throw new NullPointerException();
			}

			// Store heap.
			this.heap = heap;

			// Use self as mutex.
			this.mutex = this;
		}

		/**
		 * Constructor.
		 * 
		 * @param heap the backing heap.
		 * @param mutex the object on which to synchronize.
		 * @throws NullPointerException If <code>heap</code> or
		 *             <code>mutex</code> are <code>null</code>.
		 */
		SynchronizedHeap(final Heap<TKey, TValue> heap, final Object mutex)
				throws NullPointerException
		{
			super();

			if (heap == null || mutex == null)
			{
				throw new NullPointerException();
			}

			// Store heap and mutex.
			this.heap = heap;
			this.mutex = mutex;
		}

		/**
		 * Get the comparator used for decision in this heap.
		 * <p>
		 * If this method returns <code>null</code> then this heap uses the
		 * keys' <i>natural ordering</i>.
		 * 
		 * @return the comparator or <code>null</code>.
		 * @see java.util.Comparator
		 * @see java.lang.Comparable
		 */
		public Comparator<? super TKey> getComparator()
		{
			synchronized (this.mutex)
			{
				return this.heap.getComparator();
			}
		}

		/**
		 * Clear this heap.
		 */
		public void clear()
		{
			synchronized (this.mutex)
			{
				this.heap.clear();
			}
		}

		/**
		 * Add a mapping to this heap.
		 * 
		 * @param key the node key.
		 * @param value the node value.
		 * @return the entry created.
		 * @throws ClassCastException If the specified key is not mutually
		 *             comparable with the other keys of this heap.
		 */
		public Entry<TKey, TValue> insert(final TKey key, final TValue value)
			throws ClassCastException
		{
			synchronized (this.mutex)
			{
				return this.heap.insert(key, value);
			}
		}

		/**
		 * Insert all the entries of the specified heap into this heap.
		 * <p>
		 * The other heap will not be cleared, and this heap will simply
		 * <i>hold</i> the entries of <code>other</code>, not <i>contain</i>
		 * them.
		 * 
		 * @param other the other heap.
		 * @throws NullPointerException If <code>other</code> is
		 *             <code>null</code>.
		 * @throws ClassCastException If the keys of <code>other</code> are not
		 *             mutally comparable to the keys of this heap.
		 * @throws IllegalArgumentException If you attempt to insert a heap into
		 *             itself.
		 * @see #union(Heap)
		 */
		public void insertAll(final Heap<? extends TKey, ? extends TValue> other)
			throws NullPointerException, ClassCastException,
			IllegalArgumentException
		{
			if (other == null)
			{
				throw new NullPointerException();
			}

			if (ReferenceEquals(other, this)
					|| ReferenceEquals(other, this.heap))
			{
				throw new IllegalArgumentException();
			}

			synchronized (this.mutex)
			{
				this.insertAll(other);
			}

		}

		/**
		 * Get the number of entries in this heap.
		 * 
		 * @return the number of elements.
		 */
		public int getSize()
		{
			synchronized (this.mutex)
			{
				return this.heap.getSize();
			}
		}

		/**
		 * Is this heap empty?
		 * 
		 * @return <code>true</code> if this heap is empty; <code>false</code>
		 *         otherwise.
		 * @see #getSize()
		 */
		public boolean isEmpty()
		{
			synchronized (this.mutex)
			{
				return this.heap.isEmpty();
			}
		}

		/**
		 * Get the entry with the minimum key.
		 * <p>
		 * This method does <u>not</u> remove the returned entry.
		 * 
		 * @return the entry.
		 * @throws NoSuchElementException If this heap is empty.
		 * @see #extractMinimum()
		 */
		public Entry<TKey, TValue> getMinimum()
			throws NoSuchElementException
		{
			synchronized (this.mutex)
			{
				return this.heap.getMinimum();
			}
		}

		/**
		 * Remove and return the entry minimum key.
		 * 
		 * @return the entry.
		 * @throws NoSuchElementException If the heap is empty.
		 * @see #getMinimum()
		 */
		public Entry<TKey, TValue> extractMinimum()
			throws NoSuchElementException
		{
			synchronized (this.mutex)
			{
				return this.heap.extractMinimum();
			}
		}

		/**
		 * Decrease the key of the specified entry.
		 * 
		 * @param e the entry for which to decrease the key.
		 * @param key the new key.
		 * @throws IllegalArgumentException If <code>k</code> is larger than
		 *             <code>e</code>'s current key or <code>e</code> is not in
		 *             this heap.
		 * @throws ClassCastException If the new key is not mutually comparable
		 *             with
		 *             other keys in the heap.
		 * @throws NullPointerException If <code>e</code> is <code>null</code>.
		 */
		public void decreaseKey(final Entry<TKey, TValue> e, final TKey key)
			throws IllegalArgumentException, ClassCastException,
			NullPointerException
		{
			synchronized (this.mutex)
			{
				this.heap.decreaseKey(e, key);
			}
		}

		/**
		 * Delete the entry from this heap.
		 * 
		 * @param e the entry to delete.
		 * @throws IllegalArgumentException If <code>k</code> is larger than
		 *             <code>e</code>'s current key or <code>e</code> is not in
		 *             this heap.
		 * @throws NullPointerException If <code>e</code> is <code>null</code>.
		 */
		public void delete(final Entry<TKey, TValue> e)
			throws IllegalArgumentException, NullPointerException
		{
			synchronized (this.mutex)
			{
				this.heap.delete(e);
			}
		}

		/**
		 * Does this heap hold the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap contains the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean holdsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			synchronized (this.mutex)
			{
				return this.heap.holdsEntry(entry);
			}
		}

		/**
		 * Does this heap contain the specified entry?
		 * 
		 * @param entry the entry to check.
		 * @return <code>true</code> if this heap contains the specified entry;
		 *         <code>false</code> otherwise.
		 * @throws NullPointerException If <code>entry</code> is
		 *             <code>null</code>.
		 */
		public boolean containsEntry(final Entry<TKey, TValue> entry)
			throws NullPointerException
		{
			synchronized (this.mutex)
			{
				return this.heap.containsEntry(entry);
			}
		}

		/**
		 * Union this heap with another heap.
		 * 
		 * @param other the other heap.
		 * @throws NullPointerException If <code>other</code> is
		 *             <code>null</code>.
		 * @throws ClassCastException If the keys of the nodes are not mutally
		 *             comparable.
		 * @throws IllegalArgumentException If you attempt to union a heap with
		 *             itself.
		 */
		public void union(final Heap<TKey, TValue> other)
			throws ClassCastException, NullPointerException,
			IllegalArgumentException
		{
			synchronized (this.mutex)
			{
				this.heap.union(other);
			}
		}

		/**
		 * Equals.
		 * <p>
		 * Equals for a heap is an interesting question, since it depends not
		 * only on which elements are stored, but how they are stored. For
		 * example, it's difficult to effeciently compare a FibonacciHeap and a
		 * BinomialHeap, even if they contain the same elements, since their
		 * underlying representations are very different.
		 * <p>
		 * You may want to override the equals method in your implementation.
		 * 
		 * @param other the other object.
		 * @return <code>true</code> if this object is referentially or
		 *         semantically equal to <code>other</code>; <code>false</code>
		 *         otherwise.
		 */
		@Override
		public boolean equals(final Object other)
		{
			synchronized (this.mutex)
			{
				if (other == null)
				{
					return false;
				}

				if (this == other)
				{
					return true;
				}

				return this.heap.equals(other);
			}
		}

		/**
		 * Return a hashcode for this object that is inline with equals.
		 * <p>
		 * As mentioned in the comment for equals, equality is a difficult or
		 * perhaps interesting question for a heap to answer efficiently. If you
		 * choose to override the equals method, you must also override this
		 * method, unless you really want your objects to violate the general
		 * contract of <code>Object</code>.
		 * 
		 * @return the hashcode.
		 * @see java.lang.Object#hashCode()
		 * @see #equals(Object)
		 */
		@Override
		public int hashCode()
		{
			synchronized (this.mutex)
			{
				return this.heap.hashCode();
			}
		}

		/**
		 * Get a better string representation of this object.
		 * 
		 * @return a string representation of this object.
		 */
		@Override
		public String toString()
		{
			synchronized (this.mutex)
			{
				return this.heap.toString();
			}
		}

		/**
		 * Get the entry collection.
		 * <p>
		 * The order of the entries in the return collection is arbitrary.
		 * 
		 * @return the entry collection.
		 * @see org.teneighty.heap.Heap.Entry
		 */
		public Collection<Heap.Entry<TKey, TValue>> getEntries()
		{
			synchronized (this.mutex)
			{
				return this.heap.getEntries();
			}
		}

		/**
		 * Get the collection of values.
		 * <p>
		 * The order of the values in returned Collection is arbitrary.
		 * 
		 * @return the values.
		 */
		public Collection<TValue> getValues()
		{
			synchronized (this.mutex)
			{
				return this.heap.getValues();
			}
		}

		/**
		 * Get the collection of keys.
		 * <p>
		 * The order of the keys in returned Collection is arbitrary.
		 * 
		 * @return the keys.
		 */
		public Collection<TKey> getKeys()
		{
			synchronized (this.mutex)
			{
				return this.heap.getKeys();
			}
		}

		/**
		 * Get an iterator over the entries in this heap.
		 * 
		 * @return an iterator over the heap entries.
		 */
		public Iterator<Heap.Entry<TKey, TValue>> iterator()
		{
			synchronized (this.mutex)
			{
				return this.heap.iterator();
			}
		}

		/**
		 * Perform the specified action on each element of this heap.
		 * <p>
		 * It's extremely unwise to attempt to modify the heap (e.g. decrease
		 * the keys of all elements by one). Most implementations of this method
		 * are likely to be implemented atop an iterator over the heap, and
		 * thus, if the iterator is fail-fast and detects concurrent
		 * modification, any changes to the heap will cause the iterator to die.
		 * 
		 * @param action the action to perform.
		 * @throws NullPointerException If <code>action</code> is
		 *             <code>null</code>.
		 */
		public void forEach(final Action<Heap.Entry<TKey, TValue>> action)
			throws NullPointerException
		{
			synchronized (this.mutex)
			{
				this.heap.forEach(action);
			}
		}

		/**
		 * Serialization wave-of-the-hand.
		 * 
		 * @param out the stream to which to write.
		 * @throws IOException If serialization fails.
		 */
		private void writeObject(final ObjectOutputStream out)
			throws IOException
		{
			out.defaultWriteObject();
		}

		/**
		 * Read and restore this object from the specified stream.
		 * 
		 * @param in the stream from which to read.
		 * @throws IOException If deserialization fails.
		 * @throws ClassNotFoundException If deserialization attempts to
		 *             classload a
		 *             non-existant class.
		 */
		private void readObject(final ObjectInputStream in)
			throws IOException, ClassNotFoundException
		{
			in.defaultReadObject();

			if (this.heap == null)
			{
				throw new InvalidObjectException(
						"Backing heap is non-existant.");
			}

			// Mutex is this.
			this.mutex = this;
		}

	}

	/**
	 * Constructor. Instances of this class are not allowed, so don't bother
	 * trying.
	 * <p>
	 * Go ahead, just try to use reflection. I dare you!
	 * 
	 * @throws InternalError Always.
	 */
	private Heaps()
			throws InternalError
	{
		throw new InternalError("Instances are not allowed");
	}

}
