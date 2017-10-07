/*
 * $Id: SkewHeap.java 14 2009-11-21 18:24:37Z syenkoc $
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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * This class implements a skew heap. A skew heap is another variation on the
 * binary heap, except that we drop the constraint that the i<sup>th</sup>
 * node in the tree is the child of the (i/2)<sup>th</sup> node. Skew heaps
 * thus have a much more relaxed structure than binary heaps. Skew heaps are
 * surprisingly simple, and maintain the heap constraint through basically one
 * operation: <i>skew merge</i>, which takes two entries in the heap and links
 * them together in a parent-child relationship, with the smaller of the two
 * entries becoming the parent.
 * <p>
 * <i>Skew merge</i> isn't quite that simple - if it were, it's obvious that the
 * time bounds of the skew heap would be quite poor - so it does some additional
 * "work" on the structure of the heap. More precisely, <i>skew merge</i> is
 * implemented as follows:
 * <ol>
 * <li>Compare keys of the two entries.</li>
 * <li>Recursively merge the entry that has the larger key with the right child
 * of the other one.</li>
 * <li>Make the result entry as the right child of the entry that has smaller
 * key.</li>
 * <li>Swap the children of the new entry.</li>
 * </ol>
 * <p>
 * Using this implementation of <i>skew merge</i> amortized analysis shows that
 * all of the heap operations can be performed quite efficiently. We will not
 * give proof(s) here; instead, we'll give a high-level description of the
 * implementations of the various heap methods, leaving the question of
 * complexity as an exercise for the reader.
 * <ul>
 * <li>Insert is quite simple. We just <i>skew merge</i> the newly created entry
 * with the root.</li>
 * <li>Extract min removes the root, <i>skew merges</i> its two children, and
 * makes this the new root of the heap.</li>
 * <li>Decrease is slightly more complicated. First, we designate a replacement
 * for the entry whose key we want to decrease. This done by <i>skew merging</i>
 * the left and right children of said entry. We then set this replacement,
 * removing the entry from the heap entirely. Finally, we set the new key of the
 * entry and <i>skew merge</i> it with the root of this heap.</li>
 * <li>Delete is quite similar: We build a replacement for the entry to remove
 * and splice the replacement into the heap. (Obviously, we don't <i>skew
 * merge</i> the entry to delete back in!).</li>
 * <li>For union, we simply union the roots of the two heaps.</li>
 * </ul>
 * <p>
 * The collection-view methods of this class are backed by iterators over the
 * heap structure which are are <i>fail-fast</i>: If the heap is structurally
 * modified at any time after the iterator is created, the iterator throws a
 * <code>ConcurrentModificationException</code>. Thus, in the face of concurrent
 * modification, the iterator fails quickly and cleanly, rather than risking
 * arbitrary, non-deterministic behavior at an undetermined time in the future.
 * <p>
 * This class is not synchronized (by choice). You must ensure sequential access
 * externally, or you may damage instances of this class. Damage may be subtle
 * and difficult to detect, or it may be pronounced. You can use the
 * {@link Heaps#synchronizedHeap(Heap)} to obtain synchronized instances of this
 * class.
 * 
 * @param <TKey> The key type.
 * @param <TValue> The value type.
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 */
public class SkewHeap<TKey, TValue>
	extends AbstractLinkedHeap<TKey, TValue>
	implements Heap<TKey, TValue>, Serializable
{

	/**
	 * Serial version nonsense.
	 */
	private static final long serialVersionUID = 183483493L;

	/**
	 * The root of this heap.
	 */
	private SkewHeapEntry<TKey, TValue> root;

	/**
	 * The comparator to use for key comparisons.
	 */
	private Comparator<? super TKey> comparator;

	/**
	 * The number of elements in this heap.
	 */
	private int size;

	/**
	 * Weak back reference for containment checks and stuff.
	 */
	private HeapReference back_reference;

	/**
	 * The modification count.
	 */
	private volatile long mod_count;

	/**
	 * Constructor.
	 * <p>
	 * This constructor uses the keys' <i>natural ordering</i>.
	 */
	public SkewHeap()
	{
		this(null);
	}

	/**
	 * Constructor.
	 * <p>
	 * If <code>comparator</code> is <code>null</code>, then this heap will use
	 * the keys' <i>natural ordering</i>.
	 * 
	 * @param comparator the comparator to use for key comparisons.
	 */
	public SkewHeap(final Comparator<? super TKey> comparator)
	{
		// store stupid comparator.
		this.comparator = comparator;

		// initialize other stupid fields.
		this.size = 0;
		this.mod_count = 0;
		this.root = null;

		// create hideous weak back reference.
		this.back_reference = new HeapReference(this);
	}

	/**
	 * Get the comparator used for key comparisons in this heap.
	 * <p>
	 * If this method returns <code>null</code> then this heap uses the keys'
	 * <i>natural ordering</i>.
	 * 
	 * @return the comparator or <code>null</code>.
	 * @see java.util.Comparator
	 * @see java.lang.Comparable
	 */
	public Comparator<? super TKey> getComparator()
	{
		return this.comparator;
	}

	/**
	 * Get the number of elements in this heap.
	 * 
	 * @return the number of elements in this heap.
	 */
	public int getSize()
	{
		return this.size;
	}

	/**
	 * Add a key/value pair to this heap.
	 * 
	 * @param key the node key.
	 * @param value the node value.
	 * @return the entry created.
	 * @throws ClassCastException If the specified key is not mutually
	 *             comparable
	 *             with the other keys of this heap.
	 * @throws NullPointerException If <code>key</code> is <code>null</code> and
	 *             this heap does not support <code>null</code> keys.
	 */
	public Entry<TKey, TValue> insert(final TKey key, final TValue value)
		throws ClassCastException, NullPointerException
	{
		SkewHeapEntry<TKey, TValue> newEntry = new SkewHeapEntry<TKey, TValue>(
				key, value, this.back_reference);

		// new root = link of new + existing root. Note that link takes care of
		// nulls
		// for us.
		this.root = this.link(this.root, newEntry);

		// update size and mod count.
		this.size += 1;
		this.mod_count += 1;

		// FINIT!
		return newEntry;
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
		if (this.isEmpty())
		{
			throw new NoSuchElementException();
		}

		// just return the root.
		return this.root;
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
		if (this.isEmpty())
		{
			// die - user is DUMB.
			throw new NoSuchElementException();
		}

		// remove the root and stuff.
		SkewHeapEntry<TKey, TValue> returnValue = this.root;

		// now, link the children together.
		this.root = this.link(this.root.left, this.root.right);

		// null out parent link of root (only applies if root is not null
		// of course, which just means that this heap is empty). the link
		// method doesn't do this for us (this is a special case that really
		// only applies to this method).
		if (this.root != null)
		{
			this.root.parent = null;
		}

		// cut root links, both to the children and to this heap.
		returnValue.left = null;
		returnValue.right = null;
		returnValue.clearSourceReference();

		// update and whatnot.
		this.size -= 1;
		this.mod_count += 1;

		return returnValue;
	}

	/**
	 * Decrease the key of the given element.
	 * <p>
	 * Note that <code>e</code> must be <i>held</i> by this heap, or a
	 * <code>IllegalArgumentException</code> will be tossed.
	 * 
	 * @param e the entry for which to decrease the key.
	 * @param key the new key.
	 * @throws IllegalArgumentException If <code>k</code> is larger than
	 *             <code>e</code>'s current key or <code>e</code> is held by
	 *             this
	 *             heap.
	 * @throws ClassCastException If the new key is not mutually comparable with
	 *             other keys in the heap.
	 * @throws NullPointerException If <code>e</code> is <code>null</code>.
	 * @see #holdsEntry(Heap.Entry)
	 */
	public void decreaseKey(final Entry<TKey, TValue> e, final TKey key)
		throws IllegalArgumentException, ClassCastException,
		NullPointerException
	{
		if (e == null)
		{
			throw new NullPointerException();
		}

		// make sure key is smaller.
		if (this.compareKeys(key, e.getKey()) > 0)
		{
			throw new IllegalArgumentException();
		}

		// make sure we hold the specified entry.
		if (this.holdsEntry(e) == false)
		{
			throw new IllegalArgumentException();
		}

		// we can safely narrow - the holdsEntry method does an appropriate type
		// check.
		SkewHeapEntry<TKey, TValue> entry = (SkewHeapEntry<TKey, TValue>) e;

		if (entry == this.root)
		{
			// stupid case: entry is already the root, so we have no work
			// to do, really.
			entry.setKey(key);
			this.mod_count += 1;
			return;
		}

		// harder case: We have to cut the specified node from the heap, repair
		// the
		// damage,
		// set the new key on the entry, and then link it back with the root.

		// we know parent is not null because this is not the root entry.
		SkewHeapEntry<TKey, TValue> parent = entry.parent;
		entry.parent = null;

		// first, we cut the specified entry from this heap.
		SkewHeapEntry<TKey, TValue> left = entry.left;
		SkewHeapEntry<TKey, TValue> right = entry.right;
		entry.left = entry.right = null;

		if (left == null && right == null)
		{
			// with no children, it's pretty easy to cut this node - we just
			// have to
			// clear the parent's reference to this specified entry.
			if (parent.left == entry)
			{
				parent.left = null;
			}
			else
			{
				parent.right = null;
			}
		}
		else
		{
			// slightly harder case - we have to find/build/create a
			// replacement.

			if (left != null)
			{
				left.parent = null;
			}

			if (right != null)
			{
				right.parent = null;
			}

			// link of children forms replacement for the node to cut.
			SkewHeapEntry<TKey, TValue> replacement = this.link(left, right);
			replacement.parent = parent;

			if (parent.left == entry)
			{
				parent.left = replacement;
			}
			else
			{
				parent.right = replacement;
			}
		}

		// OK, so we've successfully cut entry from this heap, now decrease its
		// key, and link with root. We've also cleared all outgoing references,
		// so we're OK to link here...
		entry.setKey(key);
		this.root = this.link(this.root, entry);

		// update version and we're done.
		this.mod_count += 1;
	}

	/**
	 * Delete the entry from this heap.
	 * <p>
	 * Note that <code>e</code> must be <i>held</i> by this heap, or a
	 * <code>IllegalArgumentException</code> will be tossed.
	 * 
	 * @param e the entry to delete.
	 * @throws IllegalArgumentException If <code>e</code> is not held by this
	 *             heap.
	 * @throws NullPointerException If <code>e</code> is <code>null</code>.
	 * @see #holdsEntry(Heap.Entry)
	 */
	public void delete(final Entry<TKey, TValue> e)
		throws IllegalArgumentException, NullPointerException
	{
		if (e == null)
		{
			throw new NullPointerException();
		}

		// make sure we hold the specified entry.
		if (this.holdsEntry(e) == false)
		{
			throw new IllegalArgumentException();
		}

		// erased cast, but this is OK because holds does an appropriate check
		// for
		// us.
		SkewHeapEntry<TKey, TValue> entry = (SkewHeapEntry<TKey, TValue>) e;

		if (entry == this.root)
		{
			// known case - this is just an extract min!
			this.extractMinimum();
			return;
		}

		// definitely not null, since we know this isn't the minimum.
		SkewHeapEntry<TKey, TValue> parent = entry.parent;

		// we have to replace entry with the link of it's children.
		SkewHeapEntry<TKey, TValue> left = entry.left;
		SkewHeapEntry<TKey, TValue> right = entry.right;

		if (left == null && right == null)
		{
			// another special case: This node has no children. In this case,
			// it's
			// it's trivial to remove this node from the heap.
			if (parent.left == entry)
			{
				parent.left = null;
			}
			else
			{
				parent.right = null;
			}

			// and we're done with this case!
		}
		else
		{
			// clear parent refs before linking...
			if (left != null)
			{
				left.parent = null;
			}

			if (right != null)
			{
				right.parent = null;
			}

			// get replacement node by linking left and right children.
			SkewHeapEntry<TKey, TValue> replacement = this.link(left, right);
			replacement.parent = parent;

			if (parent.left == entry)
			{
				parent.left = replacement;
			}
			else
			{
				parent.right = replacement;
			}
		}

		// entry no longer held by this heap.
		entry.clearSourceReference();

		// clear stupid node pointers (for GC, but also makes debugging easier).
		entry.parent = null;
		entry.left = entry.right = null;

		// update lame metadata fields.
		this.size -= 1;
		this.mod_count += 1;
	}

	/**
	 * Union this heap with another heap.
	 * <p>
	 * Only instances of the same class are capable of being unioned together.
	 * This is a change from previous versions, when the union of different
	 * types resulting in "insertAll" type behavior. However, this meant that
	 * the union method had different semantics based on the runtime-type of the
	 * other heap, which is definitely a bad thing.
	 * <p>
	 * After a union operation, this heap will both <i>contain</i> and
	 * <i>hold</i> the entries of the other heap. The other heap is cleared in
	 * the process of union.
	 * 
	 * @param other the other heap.
	 * @throws NullPointerException If <code>other</code> is <code>null</code>.
	 * @throws ClassCastException If the keys of the nodes are not mutually
	 *             comparable or the classes do not match.
	 * @throws IllegalArgumentException If you attempt to union a heap with
	 *             itself
	 *             (i.e if <code>other == this</code>).
	 * @see #insertAll(Heap)
	 */
	@SuppressWarnings("unchecked")
	public void union(final Heap<TKey, TValue> other)
		throws ClassCastException, NullPointerException,
		IllegalArgumentException
	{
		if (other == null)
		{
			throw new NullPointerException();
		}

		if (other == this)
		{
			throw new IllegalArgumentException();
		}

		// erased cast.
		SkewHeap<TKey, TValue> that = (SkewHeap<TKey, TValue>) other;

		if (other.isEmpty())
		{
			return;
		}

		try
		{
			SkewHeapEntry<TKey, TValue> other_root = that.root;

			// new root is link of two roots.
			this.root = this.link(this.root, other_root);

			// work some back reference magic.
			that.back_reference.setHeap(this);
			that.back_reference = new HeapReference(that);

			// update size and psuedoversion.
			this.size += that.size;
			this.mod_count += 1;
		}
		finally
		{
			// always clear the other heap...
			that.clear();
		}
	}

	/**
	 * Link the specified skew heap entries, returning the new parent of the
	 * linked nodes.
	 * <p>
	 * This method is basically the workhorse of this entire class - pretty much
	 * every method that modifies the state of the heap will need to call
	 * <c>link</c>.
	 * 
	 * @param first the first node to link.
	 * @param second the second node to link.
	 * @return the new parent of the linked nodes.
	 */
	private SkewHeapEntry<TKey, TValue> link(
			final SkewHeapEntry<TKey, TValue> first,
			final SkewHeapEntry<TKey, TValue> second)
	{
		if (first == null)
		{
			// no linking needs to be done.
			return second;
		}

		if (second == null)
		{
			// again, no linking needed.
			return first;
		}

		// compare the nodes.
		SkewHeapEntry<TKey, TValue> smaller;
		SkewHeapEntry<TKey, TValue> bigger;

		if (this.compare(first, second) < 0)
		{
			smaller = first;
			bigger = second;
		}
		else
		{
			smaller = second;
			bigger = first;
		}

		// swap the left and right children of the the smaller node.
		SkewHeapEntry<TKey, TValue> tmp = smaller.right;
		smaller.right = smaller.left;

		// recursively merge the right child and the larger node.
		tmp = this.link(tmp, bigger);

		// set parent and child references.
		tmp.parent = smaller;
		smaller.left = tmp;

		// always return the smaller of the nodes of course...
		return smaller;
	}

	/**
	 * Clear this heap.
	 */
	public void clear()
	{
		this.root = null;

		// do some back reference magic.
		this.back_reference.clearHeap();
		this.back_reference = new HeapReference(this);

		// update other stupid fields.
		this.size = 0;
		this.mod_count += 1;
	}

	/**
	 * Does this heap hold the specified entry? This method returns true iff
	 * there
	 * exists some entry <code>e</code> within this heap such that
	 * <code>e == entry</code>.
	 * <p>
	 * This method can generally be implemented efficiently (i.e. using
	 * <code>O(1)</code> time) if you are clever. See the specified
	 * implementation for info on how long this operation will take.
	 * <p>
	 * Note there is a subtle, but very important, difference between this
	 * method and <code>containsEntry</code>. This method checks to see if the
	 * specified entry is held by this heap in the sense that the specific
	 * object <code>entry</code> could be reached by hopping some arbitrary set
	 * of references (be they weak, strong, etc.) starting from a strong
	 * reference directly held by this object. This is different from
	 * <code>containsEntry</code>, which checks if this heap contains an entry
	 * with exactly the same key and value values. Obviously, if a heap
	 * <i>holds</i> a specific entry it also <i>contains</i> that entry;
	 * however, the reverse is not true.
	 * 
	 * @param e the entry to check.
	 * @return <code>true</code> if this heap holds the specified entry;
	 *         <code>false</code> otherwise.
	 * @throws NullPointerException If <code>e</code> is <code>null</code>.
	 * @see #containsEntry(Heap.Entry)
	 */
	public boolean holdsEntry(final Entry<TKey, TValue> e)
		throws NullPointerException
	{
		if (e == null)
		{
			throw new NullPointerException();
		}

		// Obvious check.
		if (e.getClass().equals(SkewHeapEntry.class) == false)
		{
			return (false);
		}

		// Narrow.
		SkewHeapEntry<TKey, TValue> entry = (SkewHeapEntry<TKey, TValue>) e;

		// Use reference trickery.
		return entry.isContainedBy(this);
	}

	/**
	 * Get an iterator over the elements of this heap.
	 * 
	 * @return an iterator over the elements.
	 */
	public Iterator<Heap.Entry<TKey, TValue>> iterator()
	{
		return new EntryIterator();
	}

	/**
	 * Serialize the object to the specified output stream.
	 * <p>
	 * This method takes time <code>O(n)</code> where <code>n</code> is the size
	 * this heap.
	 * 
	 * @param out the stream to which to serialize this object.
	 * @throws IOException If this object cannot be serialized.
	 */
	private void writeObject(final ObjectOutputStream out)
		throws IOException
	{
		out.writeObject(this.comparator);
		out.writeInt(this.size);

		// Write out all key/value pairs.
		Iterator<Heap.Entry<TKey, TValue>> it = new EntryIterator();
		Heap.Entry<TKey, TValue> et = null;
		while (it.hasNext())
		{
			try
			{
				et = it.next();

				// May result in NotSerializableExceptions, but we there's not a
				// whole
				// helluva lot we can do about that.
				out.writeObject(et.getKey());
				out.writeObject(et.getValue());
			}
			catch (final ConcurrentModificationException cme)
			{
				// User's fault.
				throw (IOException) new IOException(
						"Heap structure changed during serialization")
						.initCause(cme);
			}
		}
	}

	/**
	 * Deserialize the restore this object from the specified stream.
	 * <p>
	 * This method takes time <code>O(n)</code> where <code>n</code> is the size
	 * this heap.
	 * 
	 * @param in the stream from which to read data.
	 * @throws IOException If this object cannot properly read from the
	 *             specified
	 *             stream.
	 * @throws ClassNotFoundException If deserialization tries to classload an
	 *             undefined class.
	 */
	@SuppressWarnings("unchecked")
	private void readObject(final ObjectInputStream in)
		throws IOException, ClassNotFoundException
	{
		// deserialize the comparator...
		this.comparator = (Comparator<? super TKey>) in.readObject();

		// Read the size fields.
		int rsize = in.readInt();

		// Create new ref object.
		this.back_reference = new HeapReference(this);

		// Read and insert all the keys and values.
		TKey key;
		TValue value;
		for (int index = 0; index < rsize; index++)
		{
			key = (TKey) in.readObject();
			value = (TValue) in.readObject();
			this.insert(key, value);
		}
	}

	/**
	 * Skew heap entry iterator.
	 * 
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private final class EntryIterator
		extends Object
		implements Iterator<Heap.Entry<TKey, TValue>>
	{

		/**
		 * Iterator version number.
		 */
		private final long my_mod_count;

		/**
		 * The next node.
		 * <p>
		 * We use <code>null</code> as a sentinel value to indicate that this
		 * iterator has finished.
		 */
		private SkewHeapEntry<TKey, TValue> next;

		/**
		 * Constructor.
		 */
		EntryIterator()
		{
			super();

			// initialize mod count and current/next element.
			this.my_mod_count = SkewHeap.this.mod_count;
			this.next = SkewHeap.this.root;
		}

		/**
		 * Check for concurrent modification.
		 * 
		 * @throws ConcurrentModificationException If we detect concurrent
		 *             modification.
		 */
		private void checkForConcurrentModification()
			throws ConcurrentModificationException
		{
			if (this.my_mod_count != SkewHeap.this.mod_count)
			{
				throw new ConcurrentModificationException();
			}
		}

		/**
		 * Check if this iterator has a next element.
		 * 
		 * @return true if there more elements in this iterator; false
		 *         otherwise.
		 * @throws ConcurrentModificationException If concurrent modification is
		 *             detected.
		 */
		public boolean hasNext()
			throws ConcurrentModificationException
		{
			this.checkForConcurrentModification();
			return (this.next != null);
		}

		/**
		 * Advance this iterator, returning the next value.
		 * 
		 * @return the next element in this iterator.
		 * @throws NoSuchElementException If this iterator is empty.
		 * @throws ConcurrentModificationException If concurrent modification is
		 *             detected.
		 */
		public Heap.Entry<TKey, TValue> next()
			throws NoSuchElementException, ConcurrentModificationException
		{
			if (this.hasNext() == false)
			{
				throw new NoSuchElementException();
			}

			// store current element and get the successor element.
			SkewHeapEntry<TKey, TValue> current = this.next;
			this.next = this.getSuccessor(this.next);

			// done return "next" element.
			return current;
		}

		/**
		 * Remove the current element.
		 * 
		 * @throws UnsupportedOperationException Always.
		 */
		public void remove()
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Returns the successor of the specified Entry, or <code>null</code> if
		 * none exists.
		 * 
		 * @param entry the entry.
		 * @return the next node or <code>null</code>.
		 */
		private SkewHeapEntry<TKey, TValue> getSuccessor(
				final SkewHeapEntry<TKey, TValue> entry)
		{
			if (entry == null)
			{
				return null;
			}

			if (entry.left != null)
			{
				return entry.left;
			}

			if (entry.right != null)
			{
				return entry.right;
			}

			SkewHeapEntry<TKey, TValue> parent = entry.parent;
			SkewHeapEntry<TKey, TValue> child = entry;

			// look for right child.
			while (true)
			{
				if (parent == null)
				{
					// we've reached the root - we're done here.
					return null;
				}

				// walk up until we find a left child with a right sibling.
				if (parent.left == child && parent.right != null)
				{
					return parent.right;
				}

				// keep walking up the tree.
				child = parent;
				parent = parent.parent;
			}
		}

	}

	/**
	 * A skew heap entry.
	 * 
	 * @param <TKey> The key type.
	 * @param <TValue> The value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class SkewHeapEntry<TKey, TValue>
		extends AbstractLinkedHeapEntry<TKey, TValue>
		implements Heap.Entry<TKey, TValue>, Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 98359835983L;

		/**
		 * The parent entry
		 */
		transient SkewHeapEntry<TKey, TValue> parent;

		/**
		 * The left child.
		 */
		transient SkewHeapEntry<TKey, TValue> left;

		/**
		 * The right child.
		 */
		transient SkewHeapEntry<TKey, TValue> right;

		/**
		 * Constructor.
		 * 
		 * @param key the key.
		 * @param value the value.
		 * @param ref the weak reference.
		 */
		SkewHeapEntry(final TKey key, final TValue value,
				final HeapReference ref)
		{
			super(key, value, ref);

			// initially, we have NO children.
			this.left = null;
			this.right = null;
			this.parent = null;
		}

	}

}
