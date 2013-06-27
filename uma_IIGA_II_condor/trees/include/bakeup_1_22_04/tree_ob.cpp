#include "stdafx.h"
#include "tree_ob.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
//				inline functions of binary tree and avl tree
#ifndef _TREE_ENABLE_INLINES
#undef _TREE_INLINE
#define _TREE_INLINE
#include "tree_ob.inl"
#endif

/////////////////////////////////////////////////////////////////////////////
LPVOID CObBinTree::m_lpTvParam = NULL;
CObBinTree::TV_FUNC CObBinTree::m_pfnVisit = NULL;
BOOL CObBinTree::m_bTvStop = FALSE;
/////////////////////////////////////////////////////////////////////////////
LPVOID CObAvlTree::m_lpTvParam = NULL;
CObAvlTree::TV_FUNC CObAvlTree::m_pfnVisit = NULL;
BOOL CObAvlTree::m_bTvStop = FALSE;

/*---------------------------------------------------------------------------
					implementation of binary tree
-----------------------------------------------------------------------------*/

/////////////////////////////////////////////////////////////////////////////
// CObAvlTree construction, destruction functions

CObBinTree::CObBinTree(int nBlockSize)
{
	ASSERT(nBlockSize > 0);

	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks = NULL;
	m_nBlockSize = nBlockSize;
}

void CObBinTree::RemoveAll()
{
	ASSERT_VALID(this);

	// destroy elements
	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks->FreeDataChain();
	m_pBlocks = NULL;
}

CObBinTree::~CObBinTree()
{
	RemoveAll();
	ASSERT(m_nCount == 0);
}

/////////////////////////////////////////////////////////////////////////////

POSITION CObBinTree::GetHeadPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pLChild )pNode = pNode->pLChild;

	return (POSITION) pNode;
}

POSITION CObBinTree::GetTailPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pRChild )pNode = pNode->pRChild;

	return (POSITION) pNode;
}

CObject*& CObBinTree::GetNext(POSITION& rPosition) // return *Position++
{ 
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pRChild ){
		//if the node has a right subtree, then the inorder successor is below
		pNodeNext = pNode->pRChild;
		while( pNodeNext->pLChild )pNodeNext = pNodeNext->pLChild;
	}else{
		//the inorder successor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOS)
		//or the left child of its pNodeNext pointing to the node we just come up, in this case, we find IOS.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pRChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
		
		/*pNodeNext = pNode;
		while( pNodeNext->pParent && pNodeNext->pParent->pRChild==pNodeNext ){
			pNodeNext = pNodeNext->pParent;
		}
		pNodeNext = pNodeNext->pParent;*/
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

CObject*& CObBinTree::GetPrev(POSITION& rPosition) // return *Position--
{
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pLChild ){
		//if the node has a right subtree, then the inorder predecessor is below
		pNodeNext = pNode->pLChild;
		while( pNodeNext->pRChild )pNodeNext = pNodeNext->pRChild;
	}else{
		//the inorder predecessor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOP)
		//or the right child of its pNodeNext pointing to the node we just come up, in this case, we find IOP.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pLChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

/////////////////////////////////////////////////////////////////////////////
// Node helpers
/*
 * Implementation note: CNode's are stored in CPlex blocks and
 *  chained together. Free blocks are maintained in a singly linked list
 *  using the 'pNext' member of CNode with 'm_pNodeFree' as the head.
 *  Used blocks are maintained in a doubly linked list using both 'pNext'
 *  and 'pPrev' as links and 'm_pNodeHead' and 'm_pNodeTail'
 *   as the head/tail.
 *
 * We never free a CPlex block unless the List is destroyed or RemoveAll()
 *  is used - so the total number of CPlex blocks may grow large depending
 *  on the maximum past size of the list.
 */

CObBinTree::CNode*
CObBinTree::NewNode(CObBinTree::CNode* pParent, CObBinTree::CNode* pLChild, CObBinTree::CNode* pRChild)
{
	if (m_pNodeFree == NULL)
	{
		// add another block
		CPlex* pNewBlock = CPlex::Create(m_pBlocks, m_nBlockSize,
				 sizeof(CNode));

		// chain them into free list
		CNode* pNode = (CNode*) pNewBlock->data();
		// free in reverse order to make it easier to debug
		pNode += m_nBlockSize - 1;
		for (int i = m_nBlockSize-1; i >= 0; i--, pNode--)
		{
			pNode->pRChild = m_pNodeFree;
			m_pNodeFree = pNode;
		}
	}
	ASSERT(m_pNodeFree != NULL);  // we must have something

	CObBinTree::CNode* pNode = m_pNodeFree;
	m_pNodeFree = m_pNodeFree->pRChild;

	pNode->pParent = pParent;
	pNode->pLChild = pLChild;
	pNode->pRChild = pRChild;
	m_nCount++;
	ASSERT(m_nCount > 0);  // make sure we don't overflow

	pNode->data = 0; // start with zero

	return pNode;
}

void CObBinTree::FreeNode(CObBinTree::CNode* pNode)
{
	pNode->pRChild = m_pNodeFree;
	m_pNodeFree = pNode;
	m_nCount--;
	ASSERT(m_nCount >= 0);  // make sure we don't underflow

	// if no more elements, cleanup completely
	if (m_nCount == 0)
		RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////

POSITION CObBinTree::Insert(CObject* newElement)
{
	ASSERT_VALID(this);

	CNode* pParent = m_pNodeRoot;
	CNode** ppNode = &m_pNodeRoot;

	while( *ppNode ){
		pParent = *ppNode;
		int nRet = CompareNode( (LPVOID)&pParent->data, (LPVOID)&newElement );
		if( nRet>0 ){
			ppNode = &pParent->pLChild;
		}else if( nRet<=0 ){
			ppNode = &pParent->pRChild;
		}
	}

	CNode* pNewNode = NewNode(pParent, NULL, NULL);
	pNewNode->data = newElement;
	*ppNode = pNewNode;
	return (POSITION) pNewNode;
}

void CObBinTree::RemoveAt(POSITION position)
{
	ASSERT_VALID(this);

	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));

	//case 1. single child, just bubble the child the the parent place
	//case 2. double children, bubble the in order successor to the parent place.
	CNode* pBubbleNode = NULL;
	if( pOldNode->pLChild==NULL )pBubbleNode = pOldNode->pRChild;
	else if( pOldNode->pRChild==NULL )pBubbleNode = pOldNode->pLChild;
	else{
		//case 2
		pBubbleNode = pOldNode->pRChild;
		while( pBubbleNode->pLChild )pBubbleNode = pBubbleNode->pLChild;
		//now pBubbleNode is the IOS, but if bubble node is the right child of the removing node,
		//do not change the right child of the bubble node
		if( pBubbleNode->pParent!=pOldNode ){ //the bubble node is more than one height below the removing node
			pBubbleNode->pParent->pLChild = pBubbleNode->pRChild;
			if( pBubbleNode->pRChild )pBubbleNode->pRChild->pParent=pBubbleNode->pParent;
			//link the right child of the removing node to bubble node
			pBubbleNode->pRChild = pOldNode->pRChild;
			pOldNode->pRChild->pParent = pBubbleNode;
		}
		//link the left child of the removing node to the bubble node
		pBubbleNode->pLChild = pOldNode->pLChild;
		pOldNode->pLChild->pParent = pBubbleNode;
	}
	CNode** ppNode=NULL;
	if( pOldNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pOldNode->pParent->pLChild==pOldNode )ppNode=&pOldNode->pParent->pLChild;
	else ppNode=&pOldNode->pParent->pRChild;

	*ppNode = pBubbleNode;
	if( pBubbleNode )pBubbleNode->pParent = pOldNode->pParent;

	FreeNode(pOldNode);
}

CObject* CObBinTree::RemoveRoot()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	CNode* pOldNode = m_pNodeRoot;
	CObject* returnValue = pOldNode->data;

	RemoveAt( (POSITION)m_pNodeRoot );
	return returnValue;
}

CObject* CObBinTree::RemoveHead()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetHeadPosition();
	CNode* pOldNode = (CNode*)pos;
	CObject* returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

CObject* CObBinTree::RemoveTail()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetTailPosition();
	CNode* pOldNode = (CNode*)pos;
	CObject* returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

BOOL CObBinTree::StoreInList(CObject*& pData, LPVOID lpParam)
{
	CObList& list = *((CObList*)lpParam);

	list.AddTail( pData );
	return TRUE;
}

BOOL CObBinTree::StoreInArray(CObject*& pData, LPVOID lpParam)
{
	CObArray& array = *((CObArray*)lpParam);

	array.Add( pData );
	return TRUE;
}

void CObBinTree::GetList( CObList& list )
{
	ASSERT(list.IsEmpty());
	if( !list.IsEmpty() )list.RemoveAll();

	WalkTree( StoreInList, (LPVOID)&list, TV_INORDER );

	ASSERT( list.GetCount()==GetCount() );
}

void CObBinTree::GetArray( CObArray& array )
{
	array.SetSize( 0 );
	WalkTree( StoreInArray, (LPVOID)&array, TV_INORDER );

	ASSERT( array.GetSize()==GetCount() );
}

void CObBinTree::PreOrderVisit(CObBinTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pLChild )PreOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PreOrderVisit( pNode->pRChild );
}

void CObBinTree::InOrderVisit(CObBinTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )InOrderVisit( pNode->pLChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pRChild )InOrderVisit( pNode->pRChild );
}

void CObBinTree::PostOrderVisit(CObBinTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )PostOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PostOrderVisit( pNode->pRChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
}

void CObBinTree::WalkTree( CObBinTree::TV_FUNC pfnVisit, LPVOID lpParam, int nTvOrder )
{
	ASSERT( pfnVisit!=NULL );

	if( IsEmpty() )return;

	m_lpTvParam = lpParam;
	m_pfnVisit = pfnVisit;
	m_bTvStop = FALSE;

	ASSERT( m_pNodeRoot!=NULL );
	switch(nTvOrder){
	case TV_PREORDER:
		PreOrderVisit(m_pNodeRoot);
		break;
	case TV_INORDER:
		InOrderVisit(m_pNodeRoot);
		break;
	case TV_POSTORDER:
		PostOrderVisit(m_pNodeRoot);
		break;
	default:
		ASSERT(FALSE);
	}
	m_lpTvParam = NULL;
	m_pfnVisit = NULL;
	m_bTvStop = FALSE;
}

/*POSITION CObBinTree::Find(CObject* searchValue, POSITION startAfter) const
{
	ASSERT_VALID(this);

	CNode* pNode = (CNode*) startAfter;
	if (pNode == NULL)
	{
		pNode = m_pNodeRoot;  // start at Root
	}
	else
	{
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode = pNode->pLChild;  // start after the one specified
	}

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}*/

POSITION CObBinTree::Search( CObject* searchValue ) const
{
	ASSERT_VALID(this);

	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

int CObBinTree::CompareNode( LPVOID pData1, LPVOID pData2 ) const
{
	//this function must be override;
	ASSERT(FALSE);
	return (*(CObject**)pData1)-(*(CObject**)pData2);
}

/////////////////////////////////////////////////////////////////////////////
// Serialization
BOOL CObBinTree::SerialStore(CObject*& pData, LPVOID lpParam)
{
	CArchive& ar = *((CArchive*)lpParam);

	ASSERT( ar.IsStoring() );
	ar<<pData;
	return TRUE;
}

void CObBinTree::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CObject::Serialize(ar);

	if (ar.IsStoring())
	{
		ar.WriteCount(m_nCount);
		WalkTree( SerialStore, (LPVOID)&ar, TV_PREORDER );
	}
	else
	{
		DWORD nNewCount = (DWORD)ar.ReadCount();
		CObject* newData;
		while (nNewCount--)
		{
			ar >> newData;
			Insert(newData);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void CObBinTree::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with " << m_nCount << " elements";
	if (dc.GetDepth() > 0)
	{
		POSITION pos = GetHeadPosition();
		while (pos != NULL)
			dc << "\n\t" << GetNext(pos);
	}

	dc << "\n";
}

void CObBinTree::AssertValid() const
{
	CObject::AssertValid();

	if (m_nCount == 0)
	{
		// empty BinTree
		ASSERT(m_pNodeRoot == NULL);
	}
	else
	{
		// non-empty list
		ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));
	}
}
#endif //_DEBUG

/*---------------------------------------------------------------------------
					implementation of avl tree
-----------------------------------------------------------------------------*/
/////////////////////////////////////////////////////////////////////////////
// CObAvlTree construction, destruction functions

CObAvlTree::CObAvlTree(int nBlockSize)
{
	ASSERT(nBlockSize > 0);

	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks = NULL;
	m_nBlockSize = nBlockSize;
}

void CObAvlTree::RemoveAll()
{
	ASSERT_VALID(this);

	// destroy elements
	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks->FreeDataChain();
	m_pBlocks = NULL;
}

CObAvlTree::~CObAvlTree()
{
	RemoveAll();
	ASSERT(m_nCount == 0);
}

/////////////////////////////////////////////////////////////////////////////

POSITION CObAvlTree::GetHeadPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pLChild )pNode = pNode->pLChild;

	return (POSITION) pNode;
}

POSITION CObAvlTree::GetTailPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pRChild )pNode = pNode->pRChild;

	return (POSITION) pNode;
}

CObject*& CObAvlTree::GetNext(POSITION& rPosition) // return *Position++
{ 
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pRChild ){
		//if the node has a right subtree, then the inorder successor is below
		pNodeNext = pNode->pRChild;
		while( pNodeNext->pLChild )pNodeNext = pNodeNext->pLChild;
	}else{
		//the inorder successor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOS)
		//or the left child of its pNodeNext pointing to the node we just come up, in this case, we find IOS.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pRChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
		
		/*pNodeNext = pNode;
		while( pNodeNext->pParent && pNodeNext->pParent->pRChild==pNodeNext ){
			pNodeNext = pNodeNext->pParent;
		}
		pNodeNext = pNodeNext->pParent;*/
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

CObject*& CObAvlTree::GetPrev(POSITION& rPosition) // return *Position--
{
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pLChild ){
		//if the node has a right subtree, then the inorder predecessor is below
		pNodeNext = pNode->pLChild;
		while( pNodeNext->pRChild )pNodeNext = pNodeNext->pRChild;
	}else{
		//the inorder predecessor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOP)
		//or the right child of its pNodeNext pointing to the node we just come up, in this case, we find IOP.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pLChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

/////////////////////////////////////////////////////////////////////////////
// Node helpers
/*
 * Implementation note: CNode's are stored in CPlex blocks and
 *  chained together. Free blocks are maintained in a singly linked list
 *  using the 'pNext' member of CNode with 'm_pNodeFree' as the head.
 *  Used blocks are maintained in a doubly linked list using both 'pNext'
 *  and 'pPrev' as links and 'm_pNodeHead' and 'm_pNodeTail'
 *   as the head/tail.
 *
 * We never free a CPlex block unless the List is destroyed or RemoveAll()
 *  is used - so the total number of CPlex blocks may grow large depending
 *  on the maximum past size of the list.
 */

CObAvlTree::CNode*
CObAvlTree::NewNode(CObAvlTree::CNode* pParent, CObAvlTree::CNode* pLChild, CObAvlTree::CNode* pRChild)
{
	if (m_pNodeFree == NULL)
	{
		// add another block
		CPlex* pNewBlock = CPlex::Create(m_pBlocks, m_nBlockSize,
				 sizeof(CNode));

		// chain them into free list
		CNode* pNode = (CNode*) pNewBlock->data();
		// free in reverse order to make it easier to debug
		pNode += m_nBlockSize - 1;
		for (int i = m_nBlockSize-1; i >= 0; i--, pNode--)
		{
			pNode->pRChild = m_pNodeFree;
			m_pNodeFree = pNode;
		}
	}
	ASSERT(m_pNodeFree != NULL);  // we must have something

	CObAvlTree::CNode* pNode = m_pNodeFree;
	m_pNodeFree = m_pNodeFree->pRChild;

	pNode->pParent = pParent;
	pNode->pLChild = pLChild;
	pNode->pRChild = pRChild;
	pNode->nHeight = 0;
	m_nCount++;
	ASSERT(m_nCount > 0);  // make sure we don't overflow

	pNode->data = 0; // start with zero

	return pNode;
}

void CObAvlTree::FreeNode(CObAvlTree::CNode* pNode)
{
	pNode->pRChild = m_pNodeFree;
	m_pNodeFree = pNode;
	m_nCount--;
	ASSERT(m_nCount >= 0);  // make sure we don't underflow

	// if no more elements, cleanup completely
	if (m_nCount == 0)
		RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////

POSITION CObAvlTree::Insert(CObject* newElement)
{
	ASSERT_VALID(this);

	//block 1, regular binary insertion
	CNode* pParent = m_pNodeRoot;
	CNode** ppNode = &m_pNodeRoot;

	while( *ppNode ){
		pParent = *ppNode;
		int nRet = CompareNode( (LPVOID)&newElement, (LPVOID)&pParent->data );
		if( nRet<=0 ){
			ppNode = &pParent->pLChild;
		}else if( nRet>0 ){
			ppNode = &pParent->pRChild;
		}
	}

	CNode* pNewNode = NewNode(pParent, NULL, NULL);
	pNewNode->data = newElement;
	*ppNode = pNewNode;
	
	//block 2, avl tree balance rotation
	//the loop goes upward to find the rotation node, 
	//whenever a rotation is done, STOP LOOP beause the ancestors' height are the same after rotation.
	//if the rotation is not yet found, then adjust the height of the current node.
	CNode* pRotateNode = pParent;
	while( pRotateNode ){
		if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
			//left subtree is too high, rotate to right
			CNode* pChildNode = pRotateNode->pLChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) ){
				SingleRightRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) );
				DoubleRightRotation( pRotateNode );
			}
			break;
		}else if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
			//right subtree is too high, rotate to left
			CNode* pChildNode = pRotateNode->pRChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) ){
				SingleLeftRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) );
				DoubleLeftRotation( pRotateNode );
			}
			break;
		}
		//no rotation happend here, adjust the height
		CalcHeight( pRotateNode );
		pRotateNode = pRotateNode->pParent;
	}
		
/*	CNode* pRotateNode = pParent;
	CNode* pChildNode = pNewNode;
	CNode* pGrandChdNode = pNewNode;
	
	while( pRotateNode ){
		if( pRotateNode->pLChild==pChildNode ){
			//inserted in the left subtree.
			if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
				ASSERT( pChildNode!=pGrandChdNode );
				if( pChildNode->pLChild==pGrandChdNode ){
					SingleRightRotation( pRotateNode );
				}else{
					DoubleRightRotation( pRotateNode );
				}
				break;
			}
		}else{
			//inserted in the right subtree
			if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
				ASSERT( pChildNode!=pGrandChdNode );
				if( pChildNode->pRChild==pGrandChdNode ){
					SingleRightRotation( pRotNode );
				}else{
					DoubleRightRotation( pRotNode );
				}
				break;
			}
		}
		//no rotation happend here, adjust the height
		CalcuHeight( pRotateNode );
		//change the family
		pGrandChdNode = pChildNode;
		pChildNode = pRotateNode;
		pRotateNode = pRotateNode->pParent;
	}*/

/*	CNode* pRotateNode = pParent;
	CNode* pNode = pNewNode;
	while( pRotateNode ){
		CalcuHeight( pRotateNode );
		if( pRotateNode->pLChild==pNode ){
			//inserted in the left subtree.
			if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
				int nRet = CompareNode( (LPVOID)&newElement, (LPVOID)&pNode->data );
				if( nRet<=0 ){
					SingleRightRotation( pRotNode );
				}else{
					DoubleRightRotation( pRotNode );
				}
				break;
			}
		}else{
			//inserted in the right subtree
			if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
				int nRet = CompareNode( (LPVOID)&newElement, (LPVOID)&pNode->data );
				if( nRet<=0 ){
					SingleRightRotation( pRotNode );
				}else{
					DoubleRightRotation( pRotNode );
				}
				break;
			}
		}
		//no rotation happend here, adjust the height
		CalcuHeight( pRotateNode );
		pNode = pRotateNode;
		pRotateNode = pRotateNode->pParent;
	}*/
	return (POSITION) pNewNode;

}

void CObAvlTree::RemoveAt(POSITION position)
{
	//avl tree removing. 
	//For a node having children, the concept model is to find the inorder successor of the removing node,
	//copy the data of IOS to the removing node, then remove the IOS rather than the removing node.
	//the rotation balance begins from the parent of the IOS and traces back to its ancestors.
	//however, the implementation removes the true removing node so that the iterator won't point to a garbage node.

	ASSERT_VALID(this);

	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));

	//block 1. regular binary tree remove, except makeing pRotateNode point to the start tracing node for rotation,
	//and the bubble node's height is replaced the height of the removed node
	
	//case 1. single child, just bubble the child the the parent place
	//case 2. double children, bubble the in order successor to the parent place.
	CNode* pBubbleNode = NULL;
	CNode* pRotateNode = NULL;
	if( pOldNode->pLChild==NULL ){
		pBubbleNode = pOldNode->pRChild;
		pRotateNode = pOldNode->pParent;
	}else if( pOldNode->pRChild==NULL ){
		pBubbleNode = pOldNode->pLChild;
		pRotateNode = pOldNode->pParent;
	}else{
		//case 2
		pBubbleNode = pOldNode->pRChild;
		while( pBubbleNode->pLChild )pBubbleNode = pBubbleNode->pLChild;
		//now pBubbleNode is the IOS, but if bubble node is the right child of the removing node,
		//do not change the right child of the bubble node
		if( pBubbleNode->pParent!=pOldNode ){ //the bubble node is more than one height below the removing node
			pBubbleNode->pParent->pLChild = pBubbleNode->pRChild;
			if( pBubbleNode->pRChild )pBubbleNode->pRChild->pParent=pBubbleNode->pParent;
			//save the rotation place.
			pRotateNode = pBubbleNode->pParent;
			//link the right child of the removing node to bubble node
			pBubbleNode->pRChild = pOldNode->pRChild;
			pOldNode->pRChild->pParent = pBubbleNode;
		}else{
			pRotateNode = pBubbleNode;
		}
		//link the left child of the removing node to the bubble node
		pBubbleNode->pLChild = pOldNode->pLChild;
		pOldNode->pLChild->pParent = pBubbleNode;

		//replace the height of the bubble node by the removed node
		ASSERT(AfxIsValidAddress(pBubbleNode, sizeof(CNode)));
		pBubbleNode->nHeight = pOldNode->nHeight;
	}
	CNode** ppNode=NULL;
	if( pOldNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pOldNode->pParent->pLChild==pOldNode )ppNode=&pOldNode->pParent->pLChild;
	else ppNode=&pOldNode->pParent->pRChild;

	*ppNode = pBubbleNode;
	if( pBubbleNode )pBubbleNode->pParent = pOldNode->pParent;

	FreeNode(pOldNode);
	
	//block 2, avl tree balance rotation
	//the loop goes upward to find the rotation node until the root node is reached or the current subtree's height is not changed.
	//if a rotation is done, LOOP CONTINUE beause the height of the subtree at the rotation node may decrease,
	//so the ancestors may need rotation.
	//if the rotation is not done for the node, then adjust the height of the current node.
	while( pRotateNode ){
		int nOldHeight = NODEHEIGHT(pRotateNode);
		if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
			//left subtree is too high, rotate to right
			CNode* pChildNode = pRotateNode->pLChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)>=NODEHEIGHT(pChildNode->pRChild) ){
				SingleRightRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) );
				DoubleRightRotation( pRotateNode );
			}
			//after the rotation, the pRotateNode becomes a child of the node at the rotation point
			pRotateNode = pRotateNode->pParent;
		}else if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
			//right subtree is too high, rotate to left
			CNode* pChildNode = pRotateNode->pRChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)<=NODEHEIGHT(pChildNode->pRChild) ){
				SingleLeftRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) );
				DoubleLeftRotation( pRotateNode );
			}
			//after the rotation, the pRotateNode becomes a child of the node at the rotation point
			pRotateNode = pRotateNode->pParent;
		}else{
			//no rotation happend here, adjust the height
			CalcHeight( pRotateNode );
		}

		if( nOldHeight==NODEHEIGHT(pRotateNode) ){
			//since the subtree's height is not changed, STOP LOOP since the ancestors' height is maintained.
			break;
		}
		pRotateNode = pRotateNode->pParent;
	}
}

CObject* CObAvlTree::RemoveRoot()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	CNode* pOldNode = m_pNodeRoot;
	CObject* returnValue = pOldNode->data;

	RemoveAt( (POSITION)m_pNodeRoot );
	return returnValue;
}

CObject* CObAvlTree::RemoveHead()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetHeadPosition();
	CNode* pOldNode = (CNode*)pos;
	CObject* returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

CObject* CObAvlTree::RemoveTail()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetTailPosition();
	CNode* pOldNode = (CNode*)pos;
	CObject* returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

void CObAvlTree::SingleRightRotation( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));

	CNode* pLChild = pNode->pLChild;
	ASSERT(AfxIsValidAddress(pLChild, sizeof(CNode)));

	CNode** ppNode = NULL;
	if( pNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pNode->pParent->pLChild==pNode )ppNode=&pNode->pParent->pLChild;
	else ppNode=&pNode->pParent->pRChild;

	//the right subtree of the left child becomes of the left subtree of the rotation node
	pNode->pLChild = pLChild->pRChild;
	if( pNode->pLChild )pNode->pLChild->pParent = pNode;

	//the left child becomes the root node of the subtree
	*ppNode = pLChild;
	pLChild->pParent = pNode->pParent;

	//the rotation node becomes the right child
	pLChild->pRChild = pNode;
	pNode->pParent = pLChild;

	CalcHeight(pNode);
	CalcHeight(pLChild);
}

void CObAvlTree::DoubleRightRotation( CNode* pNode )
{
	SingleLeftRotation(pNode->pLChild);
	SingleRightRotation(pNode);
}

void CObAvlTree::SingleLeftRotation( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));

	CNode* pRChild = pNode->pRChild;
	ASSERT(AfxIsValidAddress(pRChild, sizeof(CNode)));

	CNode** ppNode = NULL;
	if( pNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pNode->pParent->pLChild==pNode )ppNode=&pNode->pParent->pLChild;
	else ppNode=&pNode->pParent->pRChild;

	//the left subtree of the right child becomes of the right subtree of the rotation node
	pNode->pRChild = pRChild->pLChild;
	if( pNode->pRChild )pNode->pRChild->pParent = pNode;

	//the right child becomes the root node of the subtree
	*ppNode = pRChild;
	pRChild->pParent = pNode->pParent;

	//the rotation node becomes the left child
	pRChild->pLChild = pNode;
	pNode->pParent = pRChild;

	CalcHeight(pNode);
	CalcHeight(pRChild);
}

void CObAvlTree::DoubleLeftRotation( CNode* pNode )
{
	SingleRightRotation(pNode->pRChild);
	SingleLeftRotation(pNode);
}

void CObAvlTree::CalcHeight( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	if( NODEHEIGHT(pNode->pLChild) > NODEHEIGHT(pNode->pRChild) ){
		pNode->nHeight = NODEHEIGHT(pNode->pLChild) + 1;
	}else{
		pNode->nHeight = NODEHEIGHT(pNode->pRChild) + 1;
	}
}

BOOL CObAvlTree::StoreInList(CObject*& pData, LPVOID lpParam)
{
	CObList& list = *((CObList*)lpParam);

	list.AddTail( pData );
	return TRUE;
}

BOOL CObAvlTree::StoreInArray(CObject*& pData, LPVOID lpParam)
{
	CObArray& array = *((CObArray*)lpParam);

	array.Add( pData );
	return TRUE;
}

void CObAvlTree::GetList( CObList& list )
{
	ASSERT(list.IsEmpty());
	if( !list.IsEmpty() )list.RemoveAll();

	WalkTree( StoreInList, (LPVOID)&list, TV_INORDER );

	ASSERT( list.GetCount()==GetCount() );
}

void CObAvlTree::GetArray( CObArray& array )
{
	array.SetSize( 0 );
	WalkTree( StoreInArray, (LPVOID)&array, TV_INORDER );

	ASSERT( array.GetSize()==GetCount() );
}

void CObAvlTree::PreOrderVisit(CObAvlTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pLChild )PreOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PreOrderVisit( pNode->pRChild );
}

void CObAvlTree::InOrderVisit(CObAvlTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )InOrderVisit( pNode->pLChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pRChild )InOrderVisit( pNode->pRChild );
}

void CObAvlTree::PostOrderVisit(CObAvlTree::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )PostOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PostOrderVisit( pNode->pRChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
}

void CObAvlTree::WalkTree( CObAvlTree::TV_FUNC pfnVisit, LPVOID lpParam, int nTvOrder )
{
	ASSERT( pfnVisit!=NULL );

	if( IsEmpty() )return;

	m_lpTvParam = lpParam;
	m_pfnVisit = pfnVisit;
	m_bTvStop = FALSE;

	ASSERT( m_pNodeRoot!=NULL );
	switch(nTvOrder){
	case TV_PREORDER:
		PreOrderVisit(m_pNodeRoot);
		break;
	case TV_INORDER:
		InOrderVisit(m_pNodeRoot);
		break;
	case TV_POSTORDER:
		PostOrderVisit(m_pNodeRoot);
		break;
	default:
		ASSERT(FALSE);
	}
	m_lpTvParam = NULL;
	m_pfnVisit = NULL;
	m_bTvStop = FALSE;
}

/*POSITION CObAvlTree::Find(CObject* searchValue, POSITION startAfter) const
{
	ASSERT_VALID(this);

	CNode* pNode = (CNode*) startAfter;
	if (pNode == NULL)
	{
		pNode = m_pNodeRoot;  // start at Root
	}
	else
	{
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode = pNode->pLChild;  // start after the one specified
	}

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}*/

POSITION CObAvlTree::Search( CObject* searchValue ) const
{
	ASSERT_VALID(this);

	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

int CObAvlTree::CompareNode( LPVOID pData1, LPVOID pData2 ) const
{
	//this function must be override;
	ASSERT(FALSE);
	return (int)((*(CObject**)pData1)-(*(CObject**)pData2));
}

void CObAvlTree::CheckTree()
{
	CheckTree( m_pNodeRoot );
}

void CObAvlTree::CheckTree( CNode* pNode )
{
	if( pNode==NULL )return;

	if( pNode->pLChild )CheckTree( pNode->pLChild );
	if( pNode->pRChild )CheckTree( pNode->pRChild );
	if( abs( NODEHEIGHT(pNode->pLChild)-NODEHEIGHT(pNode->pRChild) )>=2 ){
		TRACE( "Failed balance\n" );
	}
}

/////////////////////////////////////////////////////////////////////////////
// Serialization
BOOL CObAvlTree::SerialStore(CObject*& pData, LPVOID lpParam)
{
	CArchive& ar = *((CArchive*)lpParam);

	ASSERT( ar.IsStoring() );
	ar<<pData;
	return TRUE;
}

void CObAvlTree::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CObject::Serialize(ar);

	if (ar.IsStoring())
	{
		ar.WriteCount(m_nCount);
		WalkTree( SerialStore, (LPVOID)&ar, TV_PREORDER );
	}
	else
	{
		DWORD nNewCount = (DWORD)ar.ReadCount();
		CObject* newData;
		while (nNewCount--)
		{
			ar >> newData;
			Insert(newData);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void CObAvlTree::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with " << m_nCount << " elements";
	if (dc.GetDepth() > 0)
	{
		POSITION pos = GetHeadPosition();
		while (pos != NULL)
			dc << "\n\t" << GetNext(pos);
	}

	dc << "\n";
}

void CObAvlTree::AssertValid() const
{
	CObject::AssertValid();

	if (m_nCount == 0)
	{
		// empty AvlTree
		ASSERT(m_pNodeRoot == NULL);
	}
	else
	{
		// non-empty list
		ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));
	}
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_SERIAL( CObBinTree, CObject, 0 )
IMPLEMENT_SERIAL( CObAvlTree, CObject, 0 )
/////////////////////////////////////////////////////////////////////////////
