/*
 * $Id: NaturalOrderComparator.java 14 2009-11-21 18:24:37Z syenkoc $
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
import java.util.Comparator;

/**
 * A natural order comparator.
 * 
 * @param <T> the comparator type.
 * @author Fran Lattanzio
 * @version $Revision: 14 $ $Date: 2009-11-21 13:24:37 -0500 (Sat, 21 Nov 2009) $
 */
public class NaturalOrderComparator<T extends Object & Comparable<? super T>>
	extends Object
	implements Comparator<T>, Serializable
{

	/**
	 * Serial version.
	 */
	private static final long serialVersionUID = 4583457L;

	/**
	 * Constructor.
	 */
	public NaturalOrderComparator()
	{
		super();
	}

	/**
	 * Compare two objects.
	 * 
	 * @param o1 the first object.
	 * @param o2 the second object.
	 * @return like you'd expect from a
	 *         {@link java.util.Comparator#compare(Object, Object)} call.
	 * @throws NullPointerException If <code>o1</code> or <code>o2</code> are
	 *             <code>null</code>.
	 */
	public int compare(final T o1, final T o2)
		throws NullPointerException
	{
		if (o1 == null || o2 == null)
		{
			throw new NullPointerException();
		}

		return o1.compareTo(o2);
	}

	/**
	 * Check the specified object for equality.
	 * <p>
	 * We return <code>true</code> if other has the same type as this object and
	 * <code>false</code> otherwise. (This is only reasonable definition of
	 * semantic equality for a truly stateless class).
	 * 
	 * @param other the other object.
	 * @return <code>true</code> if <code>other</code> is of the same class as
	 *         this object; <code>false</code> otherwise.
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

		return this.getClass().equals(other.getClass());
	}

	/**
	 * Get the hashcode inline with equals.
	 * <p>
	 * In accordance with the definition of equals, this returns a constant.
	 * 
	 * @return the hashcode.
	 */
	@Override
	public int hashCode()
	{
		return 1;
	}

	/**
	 * Get a (better) string representation of this object.
	 * 
	 * @return the class name, actually.
	 */
	@Override
	public String toString()
	{
		return this.getClass().getName();
	}

}
