/*
 * $Id$
 * 
 * Copyright (c) 2009 Fran Lattanzio and Jeremy Truelove
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
 * Contains various static methods for dealing with comparators.
 * 
 * @author Fran Lattanzio
 * @version $Revision$ $Date$
 */
public final class Comparators
{

	/**
	 * Invert the action of the specified comparator.
	 * 
	 * @param <T> the type of the comparator.
	 * @param comp the comparator.
	 * @return an inverted comparator.
	 * @throws NullPointerException If <code>code</code> is <code>null</code>.
	 */
	public <T> Comparator<T> invertComparator(final Comparator<T> comp)
		throws NullPointerException
	{
		if (comp == null)
		{
			throw new NullPointerException("comp");
		}

		return new InvertedComparator<T>(comp);
	}

	/**
	 * An inverted comparator.
	 * 
	 * @param <T> the comparator type.
	 * @author Fran Lattanzio
	 * @version $Revision: 6 $ $Date: 2009-10-29 23:54:44 -0400 (Thu, 29 Oct
	 *          2009) $
	 */
	private static final class InvertedComparator<T>
		extends Object
		implements Comparator<T>, Serializable
	{

		/**
		 * Serial version UID.
		 */
		private static final long serialVersionUID = 238473L;

		/**
		 * The backing comparator.
		 */
		private Comparator<T> comp;

		/**
		 * Constructor.
		 * 
		 * @param comp the comparator.
		 */
		InvertedComparator(final Comparator<T> comp)
		{
			super();

			// Store comparator.
			this.comp = comp;
		}

		/**
		 * Compare the specified objects.
		 * 
		 * @param o1 the first object
		 * @param o2 the second object.
		 * @return the opposite of what the underlying comparator does.
		 */
		public int compare(final T o1, final T o2)
		{
			return this.comp.compare(o2, o1);
		}

		/**
		 * Compare for equality.
		 * 
		 * @param other the other object.
		 * @return true if equal.
		 */
		@SuppressWarnings("unchecked")
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

			if (other.getClass().equals(InvertedComparator.class))
			{
				InvertedComparator<T> that = (InvertedComparator<T>) other;
				return this.comp.equals(that.comp);
			}

			return false;
		}

		/**
		 * Get a hashcode, inline with equals.
		 * 
		 * @return the hashcode.
		 */
		@Override
		public int hashCode()
		{
			return this.comp.hashCode();
		}

		/**
		 * Produce a happy string version of this class.
		 * 
		 * @return a string.
		 */
		@Override
		public String toString()
		{
			return String.format("Inverse of %1$s", this.comp.toString());
		}

	}

}
