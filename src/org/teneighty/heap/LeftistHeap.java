/*
 * $Id: LeftistHeap.java 14 2009-11-21 18:24:37Z syenkoc $
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
 * This class implements a leftist heap. A leftist heap is a binary tree which
 * maintains the heap invariant: The key of parent is always smaller than a key
 * of a child. The interesting part of a leftist heap, however, is that unlike,
 * say, binary heaps, which attempt to maintain balance, a leftist tries to be
 * very unbalanced. In fact, it is extremely unbalanced toward the left.
 * <p>
 * To make the notion of unbalancedness more precise, we introduce the notions
 * of null paths and null path lengths. If we consider some entry in a leftist
 * tree, call it <code>e</code>, a null path of <code>e</code> is a shortest
 * path to a leaf in the three. Null path length, then, is simply the length of
 * this path. In a leftist tree, we maintain the invariant for every node that
 * all null paths being by traversing the right node. (Note that for a leaf
 * node, we define the null path to be 0.)
 * <p>
 * The two methods <code>link</code> and <code>linkLeft</code> do the work of
 * maintaing both the heap invariant and the unbalanced invariant. The
 * <code>link</code> method links two (previously unrelated) nodes together,
 * such that the smaller of the two becomes the new parent. <code>link</code>
 * first finds the smaller of the two, then delegates to <code>linkLeft</code>,
 * which attempts to assign a child node as the left subtree of it's new parent.
 * If this is not possible (and we can determine this by looking at the null
 * path lengths), the child becomes part of the right subtree.
 * <p>
 * Most of the operations of this class are quite simple, and we described them
 * in brief detail:
 * <ul>
 * <li>Inserting a node involves creating a new entry (i.e. a tree of size one)
 * and linking it to the current minimum (root) of the tree, which takes
 * <code>O(log n)</code> time.</li>
 * <li>The union of two leftist trees is very similar - we simply union the link
 * the roots of the two heap together. This, again, takes <code>O(log n)</code>
 * time.</li>
 * <li>Extract min works by replacing the current root by the <code>link</code>
 * of its left and right subtrees.</li>
 * <li>For a decrease key call, we first cut target node from the tree, via the
 * <code>cut</code> method. The hard work here is finding a replacement for the
 * target node and repairing the damage done to the neighboring entries. We find
 * a replacement by linking the target node's left and right children, and
 * replacing the target node with this result. This can result in the parent
 * violating the null path length invariant, and if so we swap the left and
 * right children. Finally, we reset the target entry's key and link it to the
 * root.</li>
 * <li>Deletion is basically the same as a decrease key, except that the final
 * step is not resetting target's key and relinking it to the root, but rather
 * destroying it.</li>
 * </ul>
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
 * <code>insert()</code> method uses constant time, so deserialization take
 * <code>O(n log n)</code> time.
 * 
 * @param <TKey> the key type.
 * @param <TValue> the value type.
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 */
public class LeftistHeap<TKey, TValue>
	extends AbstractLinkedHeap<TKey, TValue>
	implements Heap<TKey, TValue>, Iterable<Heap.Entry<TKey, TValue>>,
	Serializable
{

	/**
	 * The serial version nonsense.
	 */
	private static final long serialVersionUID = 574934853L;

	/**
	 * Comparator.
	 */
	private Comparator<? super TKey> comp;

	/**
	 * The heap reference.
	 */
	private transient HeapReference source_heap;

	/**
	 * The root/minimum node.
	 */
	private transient LeftistHeapEntry<TKey, TValue> minimum;

	/**
	 * The size of this heap.
	 */
	private transient int size;

	/**
	 * The mod count.
	 */
	private transient volatile int mod_count;

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
	public LeftistHeap()
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
	public LeftistHeap(final Comparator<? super TKey> comp)
	{
		super();

		// Store comparator.
		this.comp = comp;

		// Create heap source reference.
		this.source_heap = new HeapReference(this);

		// Set fields, initially.
		this.minimum = null;
		this.size = 0;
		this.mod_count = 0;
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
		return this.comp;
	}

	/**
	 * Get the number of entries in this heap.
	 * 
	 * @return the mapping count.
	 */
	public int getSize()
	{
		return this.size;
	}

	/**
	 * Clear this heap.
	 */
	public void clear()
	{
		// Clear all the basic fields.
		this.minimum = null;
		this.size = 0;

		// I think this qualifies as a modification.
		this.mod_count += 1;

		// Clear source heap and recreate heap refrence.
		this.source_heap.clearHeap();
		this.source_heap = new HeapReference(this);
	}

	/**
	 * Insert the given key/value pair into this heap, returning the entry in
	 * which the new pair is stored.
	 * 
	 * @param key the key to insert.
	 * @param value the value.
	 * @return the newly created and inserted Entry.
	 * @throws ClassCastException If the key of <code>node</code> is not
	 *             mutually comparable with the keys of other nodes already in
	 *             this
	 *             heap.
	 * @throws NullPointerException If <code>node</code> is <code>null</code>.
	 */
	public Entry<TKey, TValue> insert(final TKey key, final TValue value)
		throws ClassCastException, NullPointerException
	{
		LeftistHeapEntry<TKey, TValue> lhe = new LeftistHeapEntry<TKey, TValue>(
				key, value, this.source_heap);

		// Link new entry with current minimum.
		this.minimum = this.link(this.minimum, lhe);

		// Increment size, etc.
		this.size += 1;
		this.mod_count += 1;

		// Ok, done.
		return lhe;
	}

	/**
	 * Get the entry with the minimum key.
	 * <p>
	 * This method does <u>not</u> remove the returned entry.
	 * 
	 * @return the minimum entry.
	 * @throws NoSuchElementException If this heap is empty.
	 * @see #extractMinimum()
	 */
	public Entry<TKey, TValue> getMinimum()
		throws NoSuchElementException
	{
		if (this.minimum == null)
		{
			throw new NoSuchElementException();
		}

		return this.minimum;
	}

	/**
	 * Remove and return the entry minimum key.
	 * 
	 * @return the minimum entry.
	 * @throws NoSuchElementException If the heap is empty.
	 * @see #getMinimum()
	 */
	public Entry<TKey, TValue> extractMinimum()
		throws NoSuchElementException
	{
		if (this.minimum == null)
		{
			throw new NoSuchElementException();
		}

		// Temp pointer...
		LeftistHeapEntry<TKey, TValue> min = this.minimum;

		// Replace the minimum.
		this.minimum = this.link(min.left, min.right);

		if (this.minimum != null)
		{
			// Clear parent pointer, if necessary.
			this.minimum.parent = null;
		}

		// Dec size, etc.
		this.size -= 1;
		this.mod_count += 1;

		// Clear source pointers.
		min.clearSourceReference();
		min.right = null;
		min.left = null;
		min.parent = null;

		// Ok, finit.
		return min;
	}

	/**
	 * Delete the specified entry.
	 * <p>
	 * This class can always cheaply determine of <code>e</code> is not a member
	 * of this heap (in <code>O(1)</code> time).
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
		LeftistHeapEntry<TKey, TValue> entry = (LeftistHeapEntry<TKey, TValue>) e;

		if (entry == this.minimum)
		{
			// Easy case.
			this.extractMinimum();
			return;
		}

		// Cut the entry from this heap.
		this.cut(entry);

		// Dec size, etc.
		this.size -= 1;
		this.mod_count += 1;

		// Clear source reference.
		entry.clearSourceReference();
	}

	/**
	 * Cut the specified node from this heap.
	 * <p>
	 * The specified node cannot be the root.
	 * 
	 * @param entry the entry to cut.
	 */
	private void cut(final LeftistHeapEntry<TKey, TValue> entry)
	{
		// Which side are we replacing?
		boolean left = (entry.parent.left == entry);

		// Find the replacemnet.
		LeftistHeapEntry<TKey, TValue> replacement = this.link(entry.left,
				entry.right);

		// Definitely not null...
		LeftistHeapEntry<TKey, TValue> parent = entry.parent;

		// Actually replace.
		if (left)
		{
			parent.left = replacement;
		}
		else
		{
			parent.right = replacement;
		}

		// Set parent.
		if (replacement != null)
		{
			replacement.parent = parent;
		}

		if (parent.right != null && parent.left == null)
		{
			// Easy case - parent has no left (which must be replacement, which
			// must
			// also have been null, but why check stuff we know is true?)
			LeftistHeapEntry<TKey, TValue> happy = parent.right;
			parent.right = parent.left;
			parent.left = happy;
			parent.nullPathLength = 0;
		}
		else if (parent.right != null && parent.left != null
				&& parent.right.nullPathLength > parent.left.nullPathLength)
		{
			// Swap them!
			LeftistHeapEntry<TKey, TValue> happy = parent.right;
			parent.right = parent.left;
			parent.left = happy;
			parent.nullPathLength = (parent.right.nullPathLength + 1);
		}
		else
		{
			// Parent has no right child.
			parent.nullPathLength = 0;
		}

		// Clear the node pointers.
		entry.right = null;
		entry.left = null;
		entry.parent = null;
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

		// Narrow.
		LeftistHeapEntry<TKey, TValue> x = (LeftistHeapEntry<TKey, TValue>) e;

		// Check key... May throw class cast as well.
		if (this.compareKeys(k, x.getKey()) > 0)
		{
			throw new IllegalArgumentException();
		}

		if (x == this.minimum)
		{
			// Very easy case.
			x.setKey(k);
			return;
		}

		// Cut the node from the heap.
		this.cut(x);

		// Store the new key value.
		x.setKey(k);

		// Merge node with minimum.
		this.minimum = this.link(this.minimum, x);
	}

	/**
	 * Union with another heap.
	 * <p>
	 * This operation takes <code>O(1)</code> time.
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

		if (other.getClass().equals(LeftistHeap.class))
		{
			LeftistHeap<TKey, TValue> that = (LeftistHeap<TKey, TValue>) other;

			try
			{
				// Link the root nodes... Easy enough, right? Lame javac hack
				// here.
				// Avert
				// your eyes...
				this.minimum = this.link(this.minimum, that.minimum);

				// Update stuff.
				this.size += that.size;
				this.mod_count += 1;

				// Adopt all children.
				that.source_heap.setHeap(this);

				// New heap reference for other heap.
				that.source_heap = new HeapReference(that);
			}
			finally
			{
				// Clear the other heap...
				that.clear();
			}
		}
		else
		{
			throw new ClassCastException();
		}
	}

	/**
	 * Link the specified entries, returning the entry which forms the new
	 * parent
	 * of the linked nodes.
	 * <p>
	 * Either <code>e1</code> or <code>e2</code> may be <code>null</code>; if
	 * both are, this method will throw a big fat exception (and this class has
	 * a serious programming error).
	 * 
	 * @param e1 the first entry to link.
	 * @param e2 the second entry to link.
	 * @return the entry which is now the parent of tree containing both
	 *         <code>e1</code> and <code>e2</code>.
	 */
	private LeftistHeapEntry<TKey, TValue> link(
			final LeftistHeapEntry<TKey, TValue> e1,
			final LeftistHeapEntry<TKey, TValue> e2)
	{
		if (e1 == null)
		{
			// Simple case: There's nothing to which to link!
			return e2;
		}
		else if (e2 == null)
		{
			// Same as above, but different.
			return e1;
		}
		else if (this.compare(e1, e2) < 0)
		{
			this.linkLeft(e1, e2);
			return e1;
		}
		else
		{
			this.linkLeft(e2, e1);
			return e2;
		}
	}

	/**
	 * Link <code>newleft</code> with parent, making <code>newleft</code> the
	 * new left side of <code>parent</code> (if possible). Otherwise,
	 * <code>newleft</code> will be linked with <code>parent</code>'s right
	 * side.
	 * 
	 * @param parent the parent node.
	 * @param newleft the new left side.
	 * @throws NullPointerException If <code>parent</code> or
	 *             <code>newleft</code> are <code>null</code>.
	 */
	private void linkLeft(final LeftistHeapEntry<TKey, TValue> parent,
			final LeftistHeapEntry<TKey, TValue> newleft)
		throws NullPointerException
	{
		if (parent.left == null)
		{
			// The easy case...
			parent.left = newleft;
			newleft.parent = parent;
		}
		else
		{
			// The annoying case.

			// First, link the parent's right and the new left (which isn't so
			// left
			// anymore).
			LeftistHeapEntry<TKey, TValue> newright = this.link(parent.right,
					newleft);

			// Set dumb references.
			parent.right = newright;
			newright.parent = parent;

			// Compare the null path lengths - we will want the larger null path
			// length on the left.
			if (parent.right.nullPathLength > parent.left.nullPathLength)
			{
				// Swap them!
				LeftistHeapEntry<TKey, TValue> happy = parent.right;
				parent.right = parent.left;
				parent.left = happy;
			}

			// Set the parent's null path length.
			parent.nullPathLength = (parent.right.nullPathLength + 1);
		}
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
		if (e.getClass().equals(LeftistHeapEntry.class) == false)
		{
			return false;
		}

		// Narrow.
		LeftistHeapEntry<TKey, TValue> entry = (LeftistHeapEntry<TKey, TValue>) e;

		// Use reference trickery.
		return entry.isContainedBy(this);
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
		out.defaultWriteObject();

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
		// Read non-transient fields.
		in.defaultReadObject();

		int rsize = in.readInt();

		// Create new ref object.
		this.source_heap = new HeapReference(this);

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
	 * Get an iterator over the entries of this heap.
	 * 
	 * @return an iterator over the entries of this heap.
	 */
	public Iterator<Heap.Entry<TKey, TValue>> iterator()
	{
		return new EntryIterator();
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
		private LeftistHeapEntry<TKey, TValue> next;

		/**
		 * The mod count.
		 */
		private int my_mod_count;

		/**
		 * Constructor.
		 */
		EntryIterator()
		{
			super();

			// Pay no attention to this line.
			this.next = LeftistHeap.this.minimum;

			// Traverse down to leftmost.
			if (this.next != null)
			{
				while (this.next.left != null)
				{
					this.next = this.next.left;
				}
			}

			// Copy mod count.
			this.my_mod_count = LeftistHeap.this.mod_count;
		}

		/**
		 * Does this iterator have another object?
		 * 
		 * @return <code>true</code> if this iterator has another entry;
		 *         <code>false</code> otherwise.
		 * @throws ConcurrentModificationException If concurrent modification
		 *             occurs.
		 */
		public boolean hasNext()
			throws ConcurrentModificationException
		{
			if (this.my_mod_count != LeftistHeap.this.mod_count)
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
			LeftistHeapEntry<TKey, TValue> n = this.next;
			this.next = this.getSuccessor(this.next);
			return n;
		}

		/**
		 * Not supported.
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
		private LeftistHeapEntry<TKey, TValue> getSuccessor(
				final LeftistHeapEntry<TKey, TValue> entry)
		{
			if (entry == null)
			{
				return null;
			}
			else if (entry.right != null)
			{
				LeftistHeapEntry<TKey, TValue> p = entry.right;
				while (p.left != null)
				{
					p = p.left;
				}
				return p;
			}
			else
			{
				LeftistHeapEntry<TKey, TValue> p = entry.parent;
				LeftistHeapEntry<TKey, TValue> ch = entry;
				while (p != null && ch == p.right)
				{
					ch = p;
					p = p.parent;
				}

				return p;
			}
		}

	}

	/**
	 * Leftist heap entry.
	 * 
	 * @param <K> the key type.
	 * @param <V> the value type.
	 * @author Fran Lattanzio
	 * @version $Revision: 14 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class LeftistHeapEntry<K, V>
		extends AbstractLinkedHeap.AbstractLinkedHeapEntry<K, V>
		implements Heap.Entry<K, V>, Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 547584523L;

		/**
		 * The parent node.
		 */
		transient LeftistHeapEntry<K, V> left;

		/**
		 * The sibling node.
		 */
		transient LeftistHeapEntry<K, V> right;

		/**
		 * The parent node.
		 */
		transient LeftistHeapEntry<K, V> parent;

		/**
		 * The null path length.
		 */
		transient int nullPathLength;

		/**
		 * Constructor.
		 * 
		 * @param key the key.
		 * @param value the value.
		 * @param ref the creating containing heap.
		 */
		LeftistHeapEntry(final K key, final V value, final HeapReference ref)
		{
			super(key, value, ref);
		}

	}

}
