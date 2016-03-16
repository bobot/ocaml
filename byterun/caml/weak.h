/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Damien Doligez, projet Para, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on weak arrays */

#ifndef CAML_WEAK_H
#define CAML_WEAK_H

#include "mlvalues.h"

#define Max_weak_wosize (Max_wosize - 2)

/* [len] is a value that represents a number of words (fields) */
CAMLextern value caml_ephemeron_create (mlsize_t len);

/** It is an error to call these functions not in their defined
    range. */

CAMLextern mlsize_t caml_ephemeron_key_length(value eph);
/** return the number of key in the ephemeron. The valid key offset goes
    from [0] to the predecessor of the returned value. */

CAMLextern int caml_ephemeron_get_key(value eph, mlsize_t offset, value* key);
/** return 1 if the key in the ephemeron at the given offset is set.
    Otherwise 0. When returning 1 and when [key] is non-null, set
    [*key] to the pointed value.

    The value [eph] must be an ephemeron and [offset] a valid key
    offset. If [key] is non-null, [*key] must be registered as a root.
*/

CAMLextern int caml_ephemeron_get_key_copy (value eph, mlsize_t offset,
                                            value* key);
/** Like [caml_ephemeron_get_key], but instead of setting [*key] to
    the value of the key, get a shallow copy of it.

    The value [eph] must be an ephemeron, [offset] a valid key offset
    and [k] a block. If [key] is non-null, [*key] must be registered
    as a root.
*/

CAMLextern void caml_ephemeron_unset_key(value eph, mlsize_t offset);
/** unset the key of the given ephemeron at the given offset. The
    value [eph] must be an ephemeron and [offset] a valid key offset.
*/

CAMLextern void caml_ephemeron_set_key(value eph, mlsize_t offset, value k);
/** set the key of the given ephemeron [eph] at the given offset
    [offset] with the given value [k]. The value [eph] must be an
    ephemeron, [offset] a valid key offset and [k] a block.
*/

CAMLextern void caml_ephemeron_blit_key(value eph1, mlsize_t off1,
                                        value eph2, mlsize_t off2,
                                        mlsize_t len);
/** sets the key of [eph2] with the key of [eph1]. Contrary to using
    caml_ephemeron_get_key followed by caml_ephemeron_set_key or
    caml_ephemeron_unset_key, this function does not prevent the
    incremental GC from erasing the value in its current cycle. The
    values [eph1] and [eph2] must be ephemerons and the offset between
    [off1] and [off1+len] and between [off2] and [off2+offset] must be
    valid keys of [eph1] and [eph2] respectively.
*/

CAMLextern int caml_ephemeron_get_data(value eph, value* data);
/** return 1 if the data in the ephemeron is set.  Otherwise 0. When
    returning 1 and when [data] is non-null, set [*data] to the
    pointed value.

    The value [eph] must be an ephemeron. If [data] is non-null,
    [*data] must be registered as a root.
*/


CAMLextern int caml_ephemeron_get_data_copy (value eph, value* data);
/** Like [caml_ephemeron_get_data], but instead of setting [*data] to
    the data, get a shallow copy of it.

    The value [eph] must be an ephemeron. If [data] is non-null,
    [*data] must be registered as a root.
*/

CAMLextern void caml_ephemeron_unset_data(value eph);
/** unset the data of the given ephemeron. The value [eph] must be an
    ephemeron.
*/

CAMLextern void caml_ephemeron_set_data(value eph, value k);
/** set the data of the given ephemeron [eph] with the given value
    [k]. The value [eph] must be an ephemeron and [k] a block.
*/

CAMLextern void caml_ephemeron_blit_data(value eph1, value eph2);
/** sets the data of [eph2] with the data of [eph1]. Contrary to using
    caml_ephemeron_get_data followed by caml_ephemeron_set_data or
    caml_ephemeron_unset_data, this function does not prevent the
    incremental GC from erasing the value in its current cycle. The
    values [eph1] and [eph2] must be ephemerons.
*/

#define caml_weak_array_create caml_ephemeron_create
#define caml_weak_array_length caml_ephemeron_key_length
#define caml_weak_array_get caml_ephemeron_get_key
#define caml_weak_array_get_copy caml_ephemeron_get_key_copy
#define caml_weak_array_set caml_ephemeron_set_key
#define caml_weak_array_unset caml_ephemeron_unset_key
#define caml_weak_array_blit caml_ephemeron_blit_key

#endif /* CAML_WEAK_H */
