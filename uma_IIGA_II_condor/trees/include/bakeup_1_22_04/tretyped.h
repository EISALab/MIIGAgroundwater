#ifndef __TRETYPED_H__
#define __TRETYPED_H__
/*************************************************************************************************************************
	Published version of binary tree structure 1.0
	Copyright Shengquan Yan
	The source code is not allowed to modification without the permission of the author.
	email: smyan@uiuc.edu sq_yan@hotmail.com

	See the comments in treedef.h for the usage of the tree structures
**************************************************************************************************************************/

#include "treedef.h"

/////////////////////////////////////////////////////////////////////////////
// _CTypedPtrBinTree<BASE_CLASS, TYPE>

template<class BASE_CLASS, typename TYPE, typename ARG_KEY=const TYPE&>
class _CTypedPtrBinTree : public BASE_CLASS
{
public:
// Construction
	_CTypedPtrBinTree(int nBlockSize = 10)
		: BASE_CLASS(nBlockSize) { }

	// peek at head or tail or root
	TYPE& GetRoot()
		{ return (TYPE&)BASE_CLASS::GetRoot(); }
	TYPE GetRoot() const
		{ return (TYPE&)BASE_CLASS::GetRoot(); }
	TYPE& GetHead()
		{ return (TYPE&)BASE_CLASS::GetHead(); }
	TYPE GetHead() const
		{ return (TYPE)BASE_CLASS::GetHead(); }
	TYPE& GetTail()
		{ return (TYPE&)BASE_CLASS::GetTail(); }
	TYPE GetTail() const
		{ return (TYPE)BASE_CLASS::GetTail(); }

	// get head or tail or root (and remove it) - don't call on empty list!
	// get head or tail (and remove it) - don't call on empty list!
	TYPE RemoveHead()
		{ return (TYPE)BASE_CLASS::RemoveHead(); }
	TYPE RemoveTail()
		{ return (TYPE)BASE_CLASS::RemoveTail(); }
	TYPE RemoveRoot()
		{ return (TYPE)BASE_CLASS::RemoveRoot(); }

	// iteration
	TYPE& GetNext(POSITION& rPosition)
		{ return (TYPE&)BASE_CLASS::GetNext(rPosition); }
	TYPE GetNext(POSITION& rPosition) const
		{ return (TYPE)BASE_CLASS::GetNext(rPosition); }
	TYPE& GetPrev(POSITION& rPosition)
		{ return (TYPE&)BASE_CLASS::GetPrev(rPosition); }
	TYPE GetPrev(POSITION& rPosition) const
		{ return (TYPE)BASE_CLASS::GetPrev(rPosition); }

	TYPE& GetLeft( POSITION& rPosition )
		{ return (TYPE&)BASE_CLASS::GetLeft(rPosition); }
	TYPE GetLeft( POSITION& rPosition ) const
		{ return (TYPE)BASE_CLASS::GetLeft(rPosition); }
	TYPE& GetRight( POSITION& rPosition )
		{ return (TYPE&)BASE_CLASS::GetRight(rPosition); }
	TYPE GetRight( POSITION& rPosition ) const
		{ return (TYPE)BASE_CLASS::GetRight(rPosition); }
	TYPE& GetParent( POSITION& rPosition )
		{ return (TYPE)BASE_CLASS::GetParent(rPosition); }
	TYPE GetParent( POSITION& rPosition ) const
		{ return (TYPE)BASE_CLASS::GetParent(rPosition); }

	// getting/modifying an element at a given position
	TYPE& GetAt(POSITION position)
		{ return (TYPE&)BASE_CLASS::GetAt(position); }
	TYPE GetAt(POSITION position) const
		{ return (TYPE)BASE_CLASS::GetAt(position); }
};

template<class BASE_CLASS, typename TYPE, typename ARG_KEY=const TYPE&, class ETraits=CElementTraits< TYPE, ARG_KEY > >
class CTypedPtrBinTree : public _CTypedPtrBinTree<BASE_CLASS, TYPE, ARG_KEY>
{
public:
	typedef BOOL (*TV_FUNC)(TYPE&, LPVOID lpParam);
public:
// Construction
	CTypedPtrBinTree(int nBlockSize = 10)
		: _CTypedPtrBinTree<BASE_CLASS, TYPE, ARG_KEY>(nBlockSize) { }

	// add before head or after tail
	POSITION Add(TYPE newElement)
		{ return BASE_CLASS::Add(newElement); }
	POSITION Insert(TYPE newElement)
		{ return BASE_CLASS::Insert(newElement); }

	//depth interate the tree, pfnVisit is the function used for visiting every node,
	void WalkTree( TV_FUNC pfnVisit, LPVOID lpParam=NULL, int nTvOrder=TV_INORDER )
		{ BASE_CLASS::WalkTree( (BASE_CLASS::TV_FUNC)pfnVisit, lpParam, nTvOrder ); }

	//Search is searching from root;
	//SearchByKey can accept a key value and find the corresponding node
	POSITION Search( TYPE searchValue ) const
	{ return BASE_CLASS::Search(searchValue); }
	POSITION SearchByKey( ARG_KEY keyValue ) const
	{
		ASSERT_VALID(this);
		CNode* pNode = m_pNodeRoot;

		while( pNode!=NULL ){
			int nRet = ETraits::CompareToKey( *(TYPE*)(&pNode->data), keyValue );
			if( nRet==0 )return (POSITION)pNode;
			if( nRet>0 )pNode = pNode->pLChild;
			else pNode = pNode->pRChild;
		}
		return NULL;
	}

	//Get an ordered list or array, the list should be empty before calling.
//	void GetList( CList<TYPE, ARG_TYPE>& list );
//	void GetArray( CArray<TYPE, ARG_TYPE>& array );

protected:
	virtual int CompareNode( LPVOID pData1, LPVOID pData2 ) const
	{ return ETraits::CompareElements( (TYPE&)(*(TYPE**)pData1), (TYPE&)(*(TYPE**)pData2) ); }
};


/////////////////////////////////////////////////////////////////////////////
// CTypedPtrAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>
template<class BASE_CLASS, typename TYPE, typename ARG_KEY=const TYPE&>
class _CTypedPtrAvlTree : public BASE_CLASS
{
public:

// Construction
	_CTypedPtrAvlTree(int nBlockSize = 10)
		: BASE_CLASS(nBlockSize) { }

// Attributes (head and tail)
	// peek at head or tail or root
	TYPE& GetRoot()
	{ return (TYPE&)BASE_CLASS::GetRoot(); }
	TYPE GetRoot() const
	{ return (TYPE)BASE_CLASS::GetRoot(); }
	TYPE& GetHead()
	{ return (TYPE&)BASE_CLASS::GetHead(); }
	TYPE GetHead() const
	{ return (TYPE)BASE_CLASS::GetHead(); }
	TYPE& GetTail()
	{ return (TYPE&)BASE_CLASS::GetTail(); }
	TYPE GetTail() const
	{ return (TYPE)BASE_CLASS::GetTail(); }

// Operations
	// get head or tail or root (and remove it) - don't call on empty list!
	TYPE RemoveHead()
	{ return (TYPE)BASE_CLASS::RemoveHead(); }
	TYPE RemoveTail()
	{ return (TYPE)BASE_CLASS::RemoveTail(); }
	TYPE RemoveRoot()
	{ return (TYPE)BASE_CLASS::RemoveRoot(); }

	// iteration
	TYPE& GetNext(POSITION& rPosition) // return *Position++
	{ return (TYPE&)BASE_CLASS::GetNext(rPosition); }
	TYPE GetNext(POSITION& rPosition) const // return *Position++
	{ return (TYPE)BASE_CLASS::GetNext(rPosition); }
	TYPE& GetPrev(POSITION& rPosition) // return *Position--
	{ return (TYPE&)BASE_CLASS::GetPrev(rPosition); }
	TYPE GetPrev(POSITION& rPosition) const // return *Position--
	{ return (TYPE)BASE_CLASS::GetPrev(rPosition); }

	TYPE& GetLeft( POSITION& rPosition )
	{ return (TYPE&)BASE_CLASS::GetLeft(rPosition); }
	TYPE GetLeft( POSITION& rPosition ) const
	{ return (TYPE)BASE_CLASS::GetLeft(rPosition); }
	TYPE& GetRight( POSITION& rPosition )
	{ return (TYPE&)BASE_CLASS::GetRight(rPosition); }
	TYPE GetRight( POSITION& rPosition ) const
	{ return (TYPE)BASE_CLASS::GetRight(rPosition); }
	TYPE& GetParent( POSITION& rPosition )
	{ return (TYPE&)BASE_CLASS::GetParent(rPosition); }
	TYPE GetParent( POSITION& rPosition ) const
	{ return (TYPE)BASE_CLASS::GetParent(rPosition); }

	// getting an element at a given position
	TYPE& GetAt(POSITION rPosition)
	{ return (TYPE&)BASE_CLASS::GetAt(rPosition); }
	TYPE GetAt(POSITION rPosition) const
	{ return (TYPE)BASE_CLASS::GetAt(rPosition); }
};

/////////////////////////////////////////////////////////////////////////////
// CTypedPtrAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>
template<class BASE_CLASS, typename TYPE, typename ARG_KEY=const TYPE&, class ETraits=CElementTraits< TYPE, ARG_KEY > >
class CTypedPtrAvlTree : public _CTypedPtrAvlTree<BASE_CLASS, TYPE, ARG_KEY>
{
public:
	typedef BOOL (*TV_FUNC)(TYPE&, LPVOID lpParam);
public:
// Construction
	CTypedPtrAvlTree(int nBlockSize = 10)
		: _CTypedPtrAvlTree<BASE_CLASS, TYPE, ARG_KEY>(nBlockSize) { }

	// insert or add(the same as insert) in the tree
	POSITION Add(TYPE newElement)
	{ return BASE_CLASS::Add(newElement); }
	POSITION Insert(TYPE newElement)
	{ return BASE_CLASS::Insert(newElement); }

	//depth interate the tree, pfnVisit is the function used for visiting every node,
	void WalkTree( TV_FUNC pfnVisit, LPVOID lpParam=NULL, int nTvOrder=TV_INORDER )
	{ BASE_CLASS::WalkTree( (BASE_CLASS::TV_FUNC)pfnVisit, lpParam, nTvOrder ); }

	//Search is searching from root;
	//SearchByKey can accept a key value and find the corresponding node
	POSITION Search( TYPE searchValue ) const
	{ return BASE_CLASS::Search(searchValue); }
	POSITION SearchByKey( ARG_KEY keyValue ) const
	{
		ASSERT_VALID(this);
		CNode* pNode = m_pNodeRoot;

		while( pNode!=NULL ){
			int nRet = ETraits::CompareToKey( *(TYPE*)(&pNode->data), keyValue );
			if( nRet==0 )return (POSITION)pNode;
			if( nRet>0 )pNode = pNode->pLChild;
			else pNode = pNode->pRChild;
		}
		return NULL;
	}

	//Get an ordered list or array, the list should be empty before calling.
//	void GetList( CList<TYPE, ARG_TYPE>& list );
//	void GetArray( CArray<TYPE, ARG_TYPE>& array );

protected:
	virtual int CompareNode( LPVOID pData1, LPVOID pData2 ) const
	{ return ETraits::CompareElements( (TYPE&)(*(TYPE**)pData1), (TYPE&)(*(TYPE**)pData2) ); }
};

#endif //__TRETYPED_H__