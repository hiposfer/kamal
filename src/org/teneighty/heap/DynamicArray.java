/*
 * $Id: DynamicArray.java 14 2009-11-21 18:24:37Z syenkoc $
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

import java.io.Serializable;

/**
 * A lame little helper class: Provides a real dynamic array functionality,
 * unlike the POS known as <code>Vector</code>.
 * <p>
 * It's arguable that this class should just be folded into
 * <code>BinaryHeap</code>, since that's the only class that needs
 * <code>DynamicArray</code>.
 * 
 * @param <TElement> the element type.
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 */
final class DynamicArray<TElement>
	extends Object
	implements Serializable
{

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = 874234L;

	/**
	 * Backing array of schmutz.
	 */
	private Object[] data;

	/**
	 * Constructor.
	 * 
	 * @param cap the capacity.
	 */
	DynamicArray(final int cap)
	{
		super();

		// Create data array
		this.data = new Object[cap];
	}

	/**
	 * Get the capacity of this array.
	 * 
	 * @return the capacity.
	 */
	int capacity()
	{
		return this.data.length;
	}

	/**
	 * Ensure the specified capacity.
	 * 
	 * @param new_capacity the capacity to ensure.
	 */
	void ensureCapacity(final int new_capacity)
	{
		if (new_capacity != this.capacity())
		{
			// Re-alloc all the crap.
			Object[] new_data = new Object[new_capacity];

			// Copy everything, except 0th index.
			System.arraycopy(this.data, 1, new_data, 1, Math.min(
					new_data.length, this.data.length) - 1);

			// Set new stuff.
			this.data = new_data;
		}
	}

	/**
	 * Get the element at the specified index.
	 * 
	 * @param index the index to get.
	 * @return the element at <code>index</code>.
	 * @throws ArrayIndexOutOfBoundsException If <code>index</code> is out of
	 *             bounds.
	 */
	@SuppressWarnings("unchecked")
	TElement get(final int index)
	{
		return (TElement) this.data[index];
	}

	/**
	 * Set the value at the specified index.
	 * 
	 * @param index the index.
	 * @param val the new value.
	 * @throws ArrayIndexOutOfBoundsException If <code>index</code> is out of
	 *             bounds.
	 */
	void set(final int index, final TElement val)
	{
		this.data[index] = val;
	}

	/**
	 * Clear this object.
	 * <p>
	 * Simply re-allocs the backing array.
	 * 
	 * @param cap the new capacity.
	 */
	void reallocate(int cap)
	{
		this.data = new Object[cap];
	}

}
