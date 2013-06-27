#ifndef _TREEDEF_H_
#define _TREEDEF_H_
/*************************************************************************************************************************
	Published version of binary tree structure 1.0
	Copyright Shengquan Yan
	The source code is not allowed to modification without the permission of the author.
	email: smyan@uiuc.edu sq_yan@hotmail.com

	treetmpl.h defines
		CBinTree and CAvlTree, the two general structures for standard binary tree and balanced binary tree
	tree_ob.h and tree_ob.cpp defines
		CObBinTree and CObAvlTree, the two MFC tree structures storing objects derived from CObject.
	tretyped.h defines
		CTypedPtrBinTree and CTypedPtrAvlTree, the typed safe structures, can be used for both MFC or non-MFC application,
		however, for MFC application is recommended.

	1. Use CBinTree and CAvlTree
		To use the template class, the user need to provide four types of type argument
			template< typename TYPE, typename ARG_TYPE = const TYPE&, typename ARG_KEY = ARG_TYPE, class ETraits=CElementTraits< TYPE, ARG_KEY > >
			TYPE is the data type stored into the tree node.
			ARG_TYPE tells the class functions how to pass the node data type, reference passing or direct structure passing,
				normally this should be the TYPE& for large data structure, or just TYPE for small types, eg. int or float
			ARG_KEY tell the class function SearchByKey() how to pass the key type argument.
			ETraits is trait class which provide the static comparison function to compare two arbitrary nodes, also it should include
				a comaparison function for comparing a node to a key
		A definition of ETraits class should include the comparison function, the serialization function (for MFC) and debug functions (for MFC)
			comparison function should include
				static int CompareElements( const TYPE& element1, const TYPE& element2 )
				static int CompareToKey( const TYPE& element, ARG_KEY key )
				NOTE: CompareToKey can be ignored if the SearchByKey() is never used.
			the serizalization function should include
				static void SerializeElement(CArchive& ar, TYPE& element )
			the debug function should include
				static void DumpElement( CDumpContext& dc, const TYPE& element )
			
			The default behavior of those functions have alread defined. The user should by all means override them if their behaviors are different
				eg. 
				class CMyObjTraits : public CElementTraits<MYTYPE, MYKEY>{
						static int CompareElements( const MYTYPE& elem1, const MYTYPE& elem2 )
						... 
				} (more flexible)
				or. 
				template<> class CElementTraits<MYTYPE, MYKEY>{...} (direct template override)
		Compatibility for MFC and non-MFC application
			For MFC application, all functions are workable. For non-MFC application, the following functions are disabled:
				serialization ability - Serialize()
				debug ability - AssertValid() and Dump()
				GetArray() and GetList()
		Compatibility for unix version
			For unix complier, take out the const qualifier when defining the ETraits class. The current unix complier (CC and g++)
			explains const TYPE& as TYPE const&. There will be a type cast error if the const qualifier exists in ETraits class.

	2. Use CObBinTree and CObAvlTree
		Directly using of the two classes are not recommended, the user should use the typedef safe class.
		However, if the two classed must be used, the virtual function,
			virtual int ComapreNode(LPVOID lpData1, LPVOID lpData2) const
		must be overrided, the default behavior is a directly pointer comparison, which is improper for most cases.
		NOTE: the parameter lpData1 and lpData2 are pointing to &CNode::data (CObject**)
			  SearchByKey() is not existing in the two classes since they are not template classes.

	3. Use of CTypedPtrBinTree and CTypedPtrAvlTree
		To use the template class, the user need to provide four types of type arugment
			template<class BASE_CLASS, typename TYPE, typename ARG_KEY=const TYPE&, class ETraits=CElementTraits< TYPE, ARG_KEY > >
			BASE_CLASS is the base class, it's more suitable for CObBinTree and CObAvlTree
			TYPE is the data type stored into the tree node.
			ARG_KEY tell the class function SearchByKey() how to pass the key type argument.
			ETraits is trait class which provide the static comparison function to compare two arbitrary nodes, also it should include
				a comaparison function for comparing a node to a key
			NOTE: since there is no ARG_TYPE, the bass class is better for storing small or basic type node, such as int, or pointer etc.
				CObBinTree and CObAvlTree is natural for basic class because they are storing only a pointer in the node.
		The definition of ETraits class is similar as CAvlTree and CBinTree. However, only the comparison functions are considered here.
		It's important to memorize that the design of the two typed safe classes are tailered for CObBinTree and CObAvlTree since they already
		have the serialization and debug ablity.

**************************************************************************************************************************/
#include <malloc.h>
/*---------------------------------------------------------------------------------------
									type definition
----------------------------------------------------------------------------------------*/
#define VisitTree	WalkTree			//compitable with old name VisitTree. Should use WalkTree.
#define TV_FUNC		TW_FUNC	

#ifdef _WIN32
	#include <new.h>
	#include <windows.h>
#else
	typedef int				BOOL;
	typedef void			*LPVOID;
	typedef unsigned short	WORD;
	typedef unsigned long	DWORD;

	//define data type
	#ifndef TRUE
	#define TRUE 1
	#endif

	#ifndef FALSE
	#define FALSE 0
	#endif

	#ifndef NULL
	#define NULL 0
	#endif
#endif //_WIND32

//debug macros for mfc and non-mfc
#ifndef _MFC_VER
	#ifndef ASSERT
		#if     _MSC_VER > 1000
			#include <crtdbg.h>
			#define ASSERT _ASSERT
		#else
			#include <assert.h>
			#define ASSERT assert
		#endif

		#define ASSERT_VALID(p) ASSERT(p!=NULL)
	#endif

	#define TRACE(t)
	#define AfxIsValidAddress(a, b) 1
#endif

#ifdef AFX_INLINE
	#define _TREE_INLINE	AFX_INLINE
#else
	#ifdef _DEBUG
		#define _TREE_INLINE
	#else
		#define _TREE_INLINE	inline
	#endif	//DEBUG
#endif	//AFX_INLINE

//iteration and memory helper definition
#ifndef _MFC_VER

	#ifndef _AFX_PACKING
	#define _AFX_PACKING 4
	#endif

	struct __POSITION
	{
	};
	typedef __POSITION* POSITION;

	struct CPlex     // warning variable length structure
	{
		CPlex* pNext;
	#if (_AFX_PACKING >= 8)
		DWORD dwReserved[1];    // align on 8 byte boundary
	#endif
		// BYTE data[maxNum*elementSize];

		void* data() { return this+1; }

		static CPlex* Create(CPlex*& head, size_t nMax, size_t cbElement);
				// like 'calloc' but no zero fill
				// may throw memory exceptions

		void FreeDataChain();       // free this one and links
	};

	inline CPlex* CPlex::Create( CPlex*& pHead, size_t nMax, size_t nElementSize )
	{
		CPlex* pPlex;

		ASSERT( nMax > 0 );
		ASSERT( nElementSize > 0 );

		pPlex = static_cast< CPlex* >( malloc( sizeof( CPlex )+(nMax*nElementSize) ) );
		if( pPlex == NULL )
		{
			return( NULL );
		}

		pPlex->pNext = pHead;
		pHead = pPlex;

		return( pPlex );
	}

	inline void CPlex::FreeDataChain()
	{
		CPlex* pPlex;

		pPlex = this;
		while( pPlex != NULL )
		{
			CPlex* pNext;

			pNext = pPlex->pNext;
			free( pPlex );
			pPlex = pNext;
		}
	}

	//define the empty CObject for non-MFC application
	class CObject
	{};
#else	//_MFC_VER
	#include <afxplex_.h>
	#include <afxtempl.h>
#endif	//_MFC_VER

/*---------------------------------------------------------------------------------------
								template helper class definition
----------------------------------------------------------------------------------------*/
template< typename TYPE, typename ARG_KEY=const TYPE& >
class CDefaultCompareTraits
{
public:
	static int CompareElements( const TYPE& element1, const TYPE& element2 )
	{
		TRACE("Warning, default CompareElements is called\n");
		if( element1 < element2 )
		{
			return( -1 );
		}
		else if( element1 == element2 )
		{
			return( 0 );
		}
		else
		{
			ASSERT( element1 > element2 );
			return( 1 );
		}
	}
	static int CompareToKey( const TYPE& element, ARG_KEY key )
	{
		ASSERT( FALSE );	//default, don't know how to map a element to key and compare them.
		TRACE("Warning, default CompareToKey is called\n");
/*		if( element < key ){
			return (-1);
		}else if( element == key ){
			return (0);
		}else{
			ASSERT( element > key );
			return (1);
		}*/
		return (0);
	}
};

#ifdef _MFC_VER
	template <typename TYPE>
	class CDefaultDebugTraits
	{
	public:
		static void DumpElement( CDumpContext& dc, const TYPE& element ){
			ASSERT(AfxIsValidAddress(&element, sizeof(TYPE), FALSE));
			&dc; // not used
			&element;	// not used

			// default does nothing
		}
	};

	template <typename TYPE>
	class CDefaultSerializeTraints
	{
	public:
		static void SerializeElement(CArchive& ar, TYPE& element ){
			ASSERT( AfxIsValidAddress(&element, sizeof(TYPE)) );

			// default is bit-wise read/write
			TYPE* pData = &element;

			if( ar.IsStoring() ){
				ar.Write( pData, sizeof(TYPE) );
			}else{
				ar.Read( pData, sizeof(TYPE) );
			}
		}
	};
#else	//_MFC_VER
	template <typename TYPE>
	class CDefaultDebugTraits
	{};

	template <typename TYPE>
	class CDefaultSerializeTraints
	{};
#endif	//MFC_VER

template< typename TYPE, typename ARG_KEY=const TYPE& >
class CDefaultElementTraits :
	public CDefaultSerializeTraints< TYPE >,
	public CDefaultDebugTraits< TYPE >,
	public CDefaultCompareTraits< TYPE, ARG_KEY >
{
};

template< typename TYPE, typename ARG_KEY=const TYPE& >
class CElementTraits : 
	public CDefaultElementTraits< TYPE, ARG_KEY >
{
};

#ifdef _MFC_VER
template<>
class CElementTraits<CObject*>
	: public CDefaultElementTraits<CObject*>
{
public:
	static void SerializeElement(CArchive& ar, CObject*& pObject ){
		ASSERT_VALID( pObject );

		if( ar.IsStoring() ){
			ar<<pObject;
		}else{
			CObject* newObj;
			ar>>newObj;
			pObject = newObj;
		}
	}
};
#endif

#define TV_PREORDER		0	//preorder visit the tree
#define TV_INORDER		1	//inorder visit the tree
#define TV_POSTORDER	2	//postorder visit the tree

#define NODEHEIGHT(pNode) ( (pNode) ? pNode->nHeight : -1 )

#endif