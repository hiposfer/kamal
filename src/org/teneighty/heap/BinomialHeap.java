/*
 * $Id: BinomialHeap.java 14 2009-11-21 18:24:37Z syenkoc $
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
 * This class implements a binomial heap, as described in <i>Introduction to
 * Algorithms</i>, hereafter referred to as <i>CLRS</i>.
 * <p>
 * A binomial heap is nothing more than a forest of binomial trees that
 * maintains the heap invariant (i.e. that a node's key is never less than the
 * key of it's parent). Binomial trees differ from binary trees only in their
 * structure. A binomial tree is defined recursively as:
 * <ul>
 * <li>A tree of order 0 is a single entry.</li>
 * <li>A tree of order <code>k</code> has <code>k</code> children, which are
 * binomial trees of order <code>k-1</code>, <code>k-2</code>, ... ,
 * <code>0</code>, in a list with exactly this order.</li>
 * </ul>
 * The forest is stored simply stored as an ordered list of the binomial trees.
 * This list is ordered by tree order (not by key).
 * <p>
 * Just one operation does most of the work of maintaining both the binomial
 * structure and the heap invariant: The merge operation. This operation
 * basically takes two lists of binomial trees and merges them into one master
 * list. If a tree of some order exists in one list (but not the other) we
 * simply move it over. When both lists contain a tree of the same order, we
 * link the two trees together, forming a single larger tree of the next order.
 * The orders of trees are kept such that they never exceed
 * <code>O(log n)</code>; this puts an upper bound on the merge operation. We
 * now describe the major operations in some technical detail:
 * <ul>
 * <li>Insertion works by taking a single binomial tree of order 0 (in a list of
 * size 1) and merging with the current tree list. This takes time
 * <code>O(log n)</code>.</li>
 * <li>Extracting the minimum involves first finding it (an
 * <code>O(log n)</code> operation), and yanking it from the list. We then take
 * this nodes child list, play with it a little bit, and merge it back into the
 * forest list.</li>
 * <li>Decrease key works by percolating the offending entry upwards (swapping
 * it with it's parent), until it reaches the forest list or no longer violates
 * the heap invariant.</li>
 * <li>Delete is implemented atop decrease key and extract min.</li>
 * </ul>
 * <p>
 * Part of the algorithm of a binomial heap is to exchange the key (and other
 * satellite fields) of two nodes. Thus, there is some mild crockishness with
 * the entries/nodes. Basically, I return a proxy to an entry; the proxy backer
 * can be switched (as any another "satellite" field). Thus, when you ask the
 * heap to delete, it resolves the proxy to "real" entry and deletes the "real"
 * one. It might seem stupid to do this, as we create an extra object (the
 * proxy) for each node. This is true, but it save a significant amount of time
 * during the decrease key operation by allowing us to simply swap pointer
 * references, rather than cut the entry from the tree, repair the tree, and
 * re-insert entry elsewhere. In short, I think it's a good idea.
 * <p>
 * The collection-view methods of this class are backed by iterators over the
 * heap structure which are <i>fail-fast</i>: If the heap is structurally
 * modified at any time after the iterator is created, the iterator throws a
 * <code>ConcurrentModificationException</code>. Thus, in the face of concurrent
 * modification, the iterator fails quickly and cleanly, rather than risking
 * arbitrary, non-deterministic behavior at an undetermined time in the future.
 * The collection-views returned by this class do not support the
 * <code>remove()</code> operation.
 * <p>
 * This class is not synchronized (by choice). You must ensure sequential access
 * externally, or you may damage instances of this class. Damage may be subtle
 * and difficult to detect, or it may be pronounced. You can use
 * {@link org.teneighty.heap.Heaps#synchronizedHeap(Heap)} to obtain
 * synchronized instances of this class.
 * <p>
 * Like all other heaps, the serialization mechanism of this class does not
 * serialize the full tree structure to the stream. For
 * <code>writeObject()</code>, the key/value pairs are simply written out in
 * iteration order. This tour takes total time <code>O(n)</code>, so
 * serialization takes time <code>O(n)</code> well. <code>readObject()</code>
 * reads the tuples from the stream and re-inserts them. The
 * <code>insert()</code> method uses constant time, so deserialization takes
 * <code>O(n log n)</code> time.
 * 
 * @param <TKey> the key type.
 * @param <TValue> the value type.
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 * @see "Cormen, T. H.; Leiserson C. E.; Rivest R. L. & C. Stein (2001)
 *      <i>Introduction to Algorithms</i>. MIT Press."
 */
public class BinomialHeap<TKey, TValue>
	extends AbstractLinkedHeap<TKey, TValue>
	implements Heap<TKey, TValue>, Iterable<Heap.Entry<TKey, TValue>>,
	Serializable
{

	/**
	 * The serial version.
	 */
	private static final long serialVersionUID = 458754L;

	/**
	 * Comparator.
	 */
	private Comparator<? super TKey> comp;

	/**
	 * The heap reference.
	 */
	private HeapReference source_heap;

	/**
	 * The size of this heap.
	 */
	private int size;

	/**
	 * The mod count.
	 */
	private volatile long mod_count;

	/**
	 * Head pointer.
	 */
	private BinomialHeapEntry<TKey, TValue> head;

	/**
	 * Constructor.
	 * <p>
	 * The nodes of this heap will be ordered by their keys' <i>natural
	 * ordering</i>.
	 * <p>
	 * The keys of all nodes inserted into the heap must implement the
	 * <code>Comparable</code> interface. Furthermore, all such keys must be
	 * <i>mutually comparable</i>:<code>k1.compareTo(k2)</code> must not throw a
	 * <code>ClassCastException</code> for any elements <code>k1</code> and
	 * <code>k2</code> in the heap.
	 */
	public BinomialHeap()
	{
		this(null);
	}

	/**
	 * Constructor.
	 * <p>
	 * The keys of all nodes inserted into the heap must be <i>mutually
	 * comparable</i> by the given <code>Comparator</code>:
	 * <code>comparator.compare(k1,k2)</code> must not throw a
	 * <code>ClassCastException</code> for any keys <code>k1</code> and
	 * <code>k2</code> in the heap.
	 * 
	 * @param comp the comparator to use. A <code>null</code> means the keys'
	 *            natural ordering will be used.
	 */
	public BinomialHeap(final Comparator<? super TKey> comp)
	{
		super();

		// Store comparator.
		this.comp = comp;

		// Create heap source reference.
		this.source_heap = new HeapReference(this);

		// Create root list.
		this.head = null;
	}

	/**
	 * Get the the Comparator.
	 * <p>
	 * If this method returns <code>null</code>, then this heap uses the keys'
	 * <i>natural ordering</i>.
	 * 
	 * @return the Comparator or <code>null</code>.
	 */
	public Comparator<? super TKey> getComparator()
	{
		return (this.comp);
	}

	/**
	 * Clear this heap.
	 */
	public void clear()
	{
		// Clear the root list.
		this.head = null;
		this.size = 0;
		this.mod_count += 1;
		this.source_heap.clearHeap();

		// Recreate and such.
		this.source_heap = new HeapReference(this);
	}

	/**
	 * Get the number of entries in this heap.
	 * 
	 * @return the entry count.
	 */
	public int getSize()
	{
		return this.size;
	}

	/**
	 * Get the entry with the minimum key.
	 * <p>
	 * This method does <u>not</u> remove the returned entry.
	 * <p>
	 * Code based basically exactly on CLRS.
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

		// Look through root list for smallest node...
		BinomialHeapEntry<TKey, TValue> min = this.head;
		BinomialHeapEntry<TKey, TValue> next = min.sibling;
		while (next != null)
		{
			if (this.compare(min, next) > 0)
			{
				min = next;
			}
			next = next.sibling;
		}

		return min.proxy;
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
			throw new NoSuchElementException();
		}

		// Find min and prev pointer to min...
		BinomialHeapEntry<TKey, TValue> min = this.head;
		BinomialHeapEntry<TKey, TValue> min_prev = null;
		BinomialHeapEntry<TKey, TValue> next = min.sibling;
		BinomialHeapEntry<TKey, TValue> prev = min;
		while (next != null)
		{
			if (this.compare(min, next) > 0)
			{
				min_prev = prev;
				min = next;
			}
			prev = next;
			next = next.sibling;
		}

		// Ok, time to do some work...
		// First, remove the minimum.
		if (min_prev != null)
		{
			// min is not head.
			min_prev.sibling = min.sibling;
		}
		else
		{
			// min better be head...
			this.head = min.sibling;
		}

		// Clear sibling ref!
		min.sibling = null;

		if (min.child != null)
		{
			// If min had children, we are in trouble...
			// This statement may be applicable in other contexts.
			BinomialHeapEntry<TKey, TValue> child = min.child;

			// GC aid.
			min.child = null;

			// Super lame local variables.
			BinomialHeapEntry<TKey, TValue> pv = null;
			BinomialHeapEntry<TKey, TValue> tmp = null;

			// Reverse order of child list.
			while (child != null)
			{
				tmp = child.sibling;
				child.sibling = pv;
				pv = child;

				// Clear parent ref. That was a dumb bug.
				child.parent = null;

				// Iterate...
				child = tmp;
			}

			// Victory is mine!
			child = pv;

			// new head is union of head and reversed children.
			this.head = this.unionEntries(this.head, child);
		}

		// Lame housekeeping.
		this.size -= 1;
		this.mod_count += 1;
		min.clearSourceReference();

		return min.proxy;
	}

	/**
	 * Add a mapping to this heap.
	 * 
	 * @param key the node key.
	 * @param value the node value.
	 * @return the entry created.
	 * @throws ClassCastException If the specified key is not mutually
	 *             comparable
	 *             with the other keys of this heap.
	 */
	public Entry<TKey, TValue> insert(final TKey key, final TValue value)
		throws ClassCastException
	{
		BinomialHeapEntry<TKey, TValue> entry = new BinomialHeapEntry<TKey, TValue>(
				key, value, this.source_heap);

		if (this.head == null)
		{
			this.head = entry;
			this.size = 1;
		}
		else
		{
			this.head = this.unionEntries(this.head, entry);
			this.size += 1;
		}

		this.mod_count += 1;

		// Lame proxy hack.
		HeapEntryProxy<TKey, TValue> proxy = new HeapEntryProxy<TKey, TValue>();
		proxy.entry = entry;
		entry.proxy = proxy;

		return proxy;
	}

	/**
	 * Does this heap hold the specified entry?
	 * 
	 * @param e entry to check.
	 * @throws NullPointerException If <code>e</code> is <code>null</code>.
	 * @return <code>true</code> if this heap holds <code>e</code>;
	 *         <code>false</code> otherwise.
	 */
	public boolean holdsEntry(final Heap.Entry<TKey, TValue> e)
		throws NullPointerException
	{
		if (e == null)
		{
			throw new NullPointerException();
		}

		// Obvious check.
		if (e.getClass().equals(HeapEntryProxy.class) == false)
		{
			return false;
		}

		// Narrow.
		HeapEntryProxy<TKey, TValue> proxy = (HeapEntryProxy<TKey, TValue>) e;

		// Use reference trickery.
		return proxy.entry.isContainedBy(this);
	}

	/**
	 * Link the specified entries: This function links two trees of
	 * <code>k-1</code> to form a new tree of size <code>k</code>. It does so
	 * by making <code>z</code> the parent of <code>y</code>.
	 * <p>
	 * Stolen from CLRS. And yet I feel no remorse.
	 * 
	 * @param y the first node.
	 * @param z the second node.
	 */
	private void link(final BinomialHeapEntry<TKey, TValue> y,
			final BinomialHeapEntry<TKey, TValue> z)
	{
		y.parent = z;
		y.sibling = z.child;
		z.child = y;
		z.degree += 1;
	}

	/**
	 * Union the specified entries together into one uber entry, which is
	 * returned.
	 * <p>
	 * Code based on CLRS.
	 * 
	 * @param one the first entry.
	 * @param two the second entry.
	 * @return BinomialHeapEntry{@literal <K,V>} the unioned entries.
	 */
	private BinomialHeapEntry<TKey, TValue> unionEntries(
			final BinomialHeapEntry<TKey, TValue> one,
			final BinomialHeapEntry<TKey, TValue> two)
	{
		// Merge the sibling lists into uber list increasing by degree.
		BinomialHeapEntry<TKey, TValue> newhead = this.mergeEntries(one, two);

		if (newhead == null)
		{
			return null;
		}

		// References from CLRS.
		BinomialHeapEntry<TKey, TValue> prev_x = null;
		BinomialHeapEntry<TKey, TValue> x = newhead;
		BinomialHeapEntry<TKey, TValue> next_x = x.sibling;

		while (next_x != null)
		{
			if (x.degree != next_x.degree
					|| (next_x.sibling != null && next_x.sibling.degree == x.degree))
			{
				prev_x = x;
				x = next_x;
			}
			else if (this.compare(x, next_x) <= 0)
			{
				// Same degrees, so link.
				x.sibling = next_x.sibling;
				this.link(next_x, x);
			}
			else
			{
				if (prev_x == null)
				{
					newhead = next_x;
				}
				else
				{
					prev_x.sibling = next_x;
				}

				// Link 'em up.
				this.link(x, next_x);
				x = next_x;
			}

			// Advance happy pointer.
			next_x = x.sibling;
		}

		// Ok, finit!
		return newhead;
	}

	/**
	 * Merge the specified list of sibling pointed to by the specified entries,
	 * such that the node list returned is sorted in non-decreasing order by
	 * degree.
	 * <p>
	 * This is the old list merging algorithm, as found in mergesort and such.
	 * 
	 * @param one the first list.
	 * @param two the second list.
	 * @return the merged entry.
	 */
	private BinomialHeapEntry<TKey, TValue> mergeEntries(
			BinomialHeapEntry<TKey, TValue> one,
			BinomialHeapEntry<TKey, TValue> two)
	{
		if (one == null)
		{
			return two;
		}

		if (two == null)
		{
			return one;
		}

		BinomialHeapEntry<TKey, TValue> min = null;
		if (one.degree < two.degree)
		{
			min = one;
			one = one.sibling;
		}
		else
		{
			min = two;
			two = two.sibling;
		}

		BinomialHeapEntry<TKey, TValue> last = min;

		while (one != null && two != null)
		{
			if (one.degree < two.degree)
			{
				last.sibling = one;
				one = one.sibling;
			}
			else
			{
				last.sibling = two;
				two = two.sibling;
			}

			// Cannot be null.
			last = last.sibling;
		}

		if (one == null)
		{
			last.sibling = two;
		}
		else
		{
			last.sibling = one;
		}

		return min;
	}

	/**
	 * Delete the specified entry.
	 * 
	 * @param e entry to delete.
	 * @throws IllegalArgumentException If <code>e</code> is not in this heap.
	 * @throws NullPointerException If <code>e</code> is <code>null</code>.
	 */
	public void delete(final Heap.Entry<TKey, TValue> e)
		throws IllegalArgumentException, NullPointerException
	{
		// Check and cast.
		if (this.holdsEntry(e) == false)
		{
			throw new IllegalArgumentException();
		}

		// Narrow.
		HeapEntryProxy<TKey, TValue> px = (HeapEntryProxy<TKey, TValue>) e;
		BinomialHeapEntry<TKey, TValue> entry = px.entry;

		// Make it infinitely small.
		entry.is_infinite = true;

		// Percolate the top,
		this.decreaseKeyImpl(entry);

		// Remove.
		px = (HeapEntryProxy<TKey, TValue>) this.extractMinimum();
		entry = px.entry;

		// Reset entry state.
		entry.is_infinite = false;
	}

	/**
	 * Decrease the key of the given element.
	 * <p>
	 * This class can always cheaply determine of <code>e</code> is not a member
	 * of this heap (in <code>O(1)</code> time, thanks to reference magic).
	 * 
	 * @param e the entry for which to decrease the key.
	 * @param k the new key.
	 * @throws IllegalArgumentException If <code>k</code> is larger than
	 *             <code>e</code>'s current key or <code>k</code> is not a
	 *             member
	 *             of this heap.
	 * @throws ClassCastException If the new key is not mutually comparable with
	 *             other keys in the heap.
	 */
	public void decreaseKey(final Heap.Entry<TKey, TValue> e, final TKey k)
		throws IllegalArgumentException, ClassCastException
	{
		// Check and cast.
		if (this.holdsEntry(e) == false)
		{
			throw new IllegalArgumentException();
		}

		// x from CLRS.
		HeapEntryProxy<TKey, TValue> px = (HeapEntryProxy<TKey, TValue>) e;
		BinomialHeapEntry<TKey, TValue> x = px.entry;

		// Check key... May throw class cast as well.
		if (this.compareKeys(k, x.getKey()) > 0)
		{
			throw new IllegalArgumentException();
		}

		// Store the new key value.
		x.setKey(k);

		// Restore the heap structure.
		this.decreaseKeyImpl(x);
	}

	/**
	 * Decrease key implementation. Basically, we restore the heap structure by
	 * percolating <code>x</code> up the heap.
	 * 
	 * @param x the whose key has just been decreased and needs to be percolated
	 *            toward the top of the heap.
	 */
	private void decreaseKeyImpl(final BinomialHeapEntry<TKey, TValue> x)
	{
		// Happy variables from .
		BinomialHeapEntry<TKey, TValue> y = x;
		BinomialHeapEntry<TKey, TValue> z = y.parent;

		TKey key;
		TValue val;
		boolean inf;
		HeapEntryProxy<TKey, TValue> prx = null;

		while (z != null && this.compare(y, z) < 0)
		{
			key = y.getKey();
			val = y.getValue();
			inf = y.is_infinite;
			prx = y.proxy;

			// Set y's props.
			y.setKey(z.getKey());
			y.setValue(z.getValue());
			y.is_infinite = z.is_infinite;
			y.proxy = z.proxy;
			y.proxy.entry = y;

			// z time.
			z.setKey(key);
			z.setValue(val);
			z.is_infinite = inf;
			z.proxy = prx;
			z.proxy.entry = z;

			// Continue iteration
			y = z;
			z = y.parent;
		}
	}

	/**
	 * Union with another heap.
	 * <p>
	 * This operation takes time <code>O(log n)</code> if <code>other</code> is
	 * an instance of <code>BinomialHeap</code> and <code>O(n log n)</code> time
	 * otherwise.
	 * 
	 * @param other the other heap.
	 * @throws NullPointerException If <code>other</code> is <code>null</code>.
	 * @throws ClassCastException If the keys of the nodes are not mutally
	 *             comparable.
	 * @throws IllegalArgumentException If you attempt to union a heap with
	 *             itself.
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

		if (this == other)
		{
			throw new IllegalArgumentException();
		}

		if (other.isEmpty())
		{
			return;
		}

		if (other.getClass().equals(BinomialHeap.class))
		{
			// Get other root.
			BinomialHeap<TKey, TValue> that = (BinomialHeap) other;

			try
			{
				// Simple check for comparability...
				// Might throw CCE.
				this.compare(this.head, that.head);

				// Union root lists.
				this.head = this.unionEntries(that.head, this.head);

				// Update heap fields.
				this.size += that.size;
				this.mod_count += 1;

				// Change that heap's heap reference to point to this heap.
				// Thus, all child of that become children of this.
				that.source_heap.setHeap(this);
				that.source_heap = new HeapReference(that);
			}
			finally
			{
				// Clear other heap regardless...
				that.clear();
			}

		}
		else
		{
			throw new ClassCastException();
		}
	}

	/**
	 * Get an iterator over this heap entry set.
	 * 
	 * @return an iterator over the entry set.
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
		// Write non-transient fields.
		out.writeInt(this.size);

		// Write out all key/value pairs.
		Iterator<Heap.Entry<TKey, TValue>> it = this.iterator();
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
	 * This method takes time <code>O(n log n)</code> where <code>n</code> is
	 * the size this heap.
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
		// Read non-transient fields.
		int rsize = in.readInt();

		// Create new ref object.
		this.source_heap = new HeapReference(this);
		this.mod_count = 0;

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
	 * Entry iterator class.
	 * <p>
	 * This iterator does not support the <code>remove()</code> operation. Any
	 * call to <code>remove()</code> will fail with a
	 * <code>UnsupportedOperationException</code>.
	 * 
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private class EntryIterator
		extends Object
		implements Iterator<Heap.Entry<TKey, TValue>>
	{

		/**
		 * The next entry.
		 */
		private BinomialHeapEntry<TKey, TValue> next;

		/**
		 * Local modification count.
		 */
		private final long my_mod_count;

		/**
		 * Constructor.
		 */
		EntryIterator()
		{
			super();

			// Start at min.
			this.next = BinomialHeap.this.head;

			// Copy mod count.
			this.my_mod_count = BinomialHeap.this.mod_count;
		}

		/**
		 * Does this iterator have another object?
		 * 
		 * @return <code>true</code> if there's another object;
		 *         <code>false</code> otherwise.
		 * @throws ConcurrentModificationException If concurrent modification
		 *             occurs.
		 */
		public boolean hasNext()
		{
			if (this.my_mod_count != BinomialHeap.this.mod_count)
			{
				throw new ConcurrentModificationException();
			}

			return (this.next != null);
		}

		/**
		 * Get the next object from this iterator.
		 * 
		 * @return the next object.
		 * @throws NoSuchElementException If the iterator has no more elements.
		 * @throws ConcurrentModificationException If concurrent modification
		 *             occurs.
		 */
		public Heap.Entry<TKey, TValue> next()
			throws NoSuchElementException, ConcurrentModificationException
		{
			if (this.hasNext() == false)
			{
				throw new NoSuchElementException();
			}

			// Get the next node.
			BinomialHeapEntry<TKey, TValue> n = this.next;
			this.next = this.eulerianSuccessor(this.next);
			return n.proxy;
		}

		/**
		 * Not supported.
		 * 
		 * @throws UnsupportedOperationException always.
		 */
		public void remove()
			throws UnsupportedOperationException
		{
			throw new UnsupportedOperationException();
		}

		/**
		 * Return the successor entry to the specified entry, in the context of
		 * Euler tour over this heap.
		 * <p>
		 * This is so named because I might someday support alternate iteration
		 * algorithms/styles, e.g. pre-order, post-order, and so forth into tour
		 * infinity.
		 * 
		 * @param entry the given entry.
		 * @return the successor entry.
		 */
		private BinomialHeapEntry<TKey, TValue> eulerianSuccessor(
				final BinomialHeapEntry<TKey, TValue> entry)
		{
			if (entry == null)
			{
				// Not much we can do here...
				return null;
			}
			else if (entry.child != null)
			{
				return entry.child;
			}
			else if (entry.parent == null)
			{
				return entry.sibling;
			}
			else
			{
				// Could also be null, when you think about it!
				return entry.parent.sibling;
			}
		}

	}

	/**
	 * Node proxy class.
	 * 
	 * @param <TKey> the key type.
	 * @param <TValue> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class HeapEntryProxy<TKey, TValue>
		extends Object
		implements Heap.Entry<TKey, TValue>, Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 23497862L;

		/**
		 * Backing binomial heap entry.
		 */
		BinomialHeapEntry<TKey, TValue> entry;

		/**
		 * Constructor.
		 * <p>
		 * Does nothing.
		 */
		HeapEntryProxy()
		{
			super();
		}

		/**
		 * Compare this object for equality with the specified object.
		 * 
		 * @param other the other object.
		 * @return <code>true</code> if equal; <code>false</code> otherwise.
		 */
		@Override
		public boolean equals(final Object other)
		{
			if (other == null)
			{
				return (false);
			}

			if (other == this)
			{
				return (true);
			}

			return this.entry.equals(other);
		}

		/**
		 * Get a hashcode inline with equals.
		 * 
		 * @return the hashcode.
		 */
		@Override
		public int hashCode()
		{
			return this.entry.hashCode();
		}

		/**
		 * Get the key.
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
		 * @param value the new value.
		 * @return the old value.
		 */
		public TValue setValue(final TValue value)
		{
			return this.entry.setValue(value);
		}

		/**
		 * Get nice(r) string representation of this object.
		 * 
		 * @return a better string.
		 */
		@Override
		public String toString()
		{
			return this.entry.toString();
		}

		/**
		 * Deserialize the restore this object from the specified stream.
		 * 
		 * @param in the stream from which to read data.
		 * @throws IOException If this object cannot properly read from the
		 *             specified stream.
		 * @throws ClassNotFoundException If deserialization tries to classload
		 *             an
		 *             undefined class.
		 */
		private void readObject(final ObjectInputStream in)
			throws IOException, ClassNotFoundException
		{
			// Read non-transient fields.
			in.defaultReadObject();

			// Reference proxy back to this...
			this.entry.proxy = this;
		}

	}

	/**
	 * Binomial heap entry.
	 * 
	 * @param <K> the key type.
	 * @param <V> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class BinomialHeapEntry<K, V>
		extends AbstractLinkedHeapEntry<K, V>
		implements Heap.Entry<K, V>, Serializable
	{

		/**
		 * Serial version.
		 */
		private static final long serialVersionUID = 93424L;

		/**
		 * The degree of this node.
		 */
		transient int degree;

		/**
		 * Sibling pointer.
		 */
		transient BinomialHeapEntry<K, V> sibling;

		/**
		 * Parent entry.
		 */
		transient BinomialHeapEntry<K, V> parent;

		/**
		 * Children pointer.
		 */
		transient BinomialHeapEntry<K, V> child;

		/**
		 * Proxy reference.
		 */
		transient HeapEntryProxy<K, V> proxy;

		/**
		 * Constructor.
		 * 
		 * @param key the key
		 * @param value the value.
		 * @param ref the reference.
		 */
		BinomialHeapEntry(final K key, final V value, final HeapReference ref)
		{
			super(key, value, ref);

			// Blah - store some stuff.
			this.sibling = null;
			this.child = null;
			this.parent = null;
			this.degree = 0;
		}

	}

}
