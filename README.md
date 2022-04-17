Unofficial mirror of the FORTRAN 77 IAU SOFA library. The original source is distributed as zip archives. This repository is a reconstructed git history of each release.

### Terms and Conditions

Text equivalent to the following appears at the end of every SOFA routine (with one exception).

The one exception is the "leap second" routine DAT. This uniquely is classified as "user replaceable", and has a mitigated license statement that permits the distribution of local variants under the same name. This measure allows other SOFA routines to call the local variant, which may be file or network based, or otherwise equipped to pick up IERS leap second updates with no need to download new SOFA code.

### Copyright

The copyright of the SOFA Software belongs to the Standards Of Fundamental Astronomy Board of the International Astronomical Union.

### SOFA Software License

By using this software you accept the following six terms and conditions which apply to its use.

1. The Software is owned by the IAU SOFA Board ("SOFA").
2. Permission is granted to anyone to use the SOFA software for any purpose, including commercial applications, free of charge and without payment of royalties, subject to the conditions and restrictions listed below.
3. You (the user) may copy and distribute SOFA source code to others, and use and adapt its code and algorithms in your own software, on a world-wide, royalty-free basis. That portion of your distribution that does not consist of intact and unchanged copies of SOFA source code files is a "derived work" that must comply with the following requirements:
  * Your work shall be marked or carry a statement that it (i) uses routines and computations derived by you from software provided by SOFA under license to you; and (ii) does not itself constitute software provided by and/or endorsed by SOFA.
  * The source code of your derived work must contain descriptions of how the derived work is based upon, contains and/or differs from the original SOFA software.
  * The names of all routines in your derived work shall not include the prefix "iau" or "sofa" or trivial modifications thereof such as changes of case.
  * The origin of the SOFA components of your derived work must not be misrepresented; you must not claim that you wrote the original software, nor file a patent application for SOFA software or algorithms embedded in the SOFA software.
  * These requirements must be reproduced intact in any source distribution and shall apply to anyone to whom you have granted a further right to modify the source code of your derived work.
Note that, as originally distributed, the SOFA software is intended to be a definitive implementation of the IAU standards, and consequently third-party modifications are discouraged. All variations, no matter how minor, must be explicitly marked as such, as explained above.
4. You shall not cause the SOFA software to be brought into disrepute, either by misuse, or use for inappropriate tasks, or by inappropriate modification.
5. The SOFA software is provided "as is" and SOFA makes no warranty as to its use or performance. SOFA does not and cannot warrant the performance or results which the user may obtain by using the SOFA software. SOFA makes no warranties, express or implied, as to non-infringement of third party rights, merchantability, or fitness for any particular purpose. In no event will SOFA be liable to the user for any consequential, incidental, or special damages, including any lost profits or lost savings, even if a SOFA representative has been advised of such damages, or for any claim by any third party.
6. The provision of any version of the SOFA software under the terms and conditions specified herein does not imply that future versions will also be made available under the same terms and conditions.

### See also

 * [Standards of Fundamental Astronomy](http://www.iausofa.org/tandc.html) (original source)
 * [astro-fortran](https://github.com/jacobwilliams/astro-fortran) -- Modern Fortran implementation of the same library.
