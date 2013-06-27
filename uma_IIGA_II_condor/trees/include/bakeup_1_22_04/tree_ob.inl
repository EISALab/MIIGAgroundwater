#ifdef _TREE_INLINE

//-------------------------------------------------------------------------------
//							binary tree inline functions
//-------------------------------------------------------------------------------

_TREE_INLINE int CObBinTree::GetCount() const
	{ return m_nCount; }
_TREE_INLINE BOOL CObBinTree::IsEmpty() const
	{ return m_nCount == 0; }
_TREE_INLINE CObject*& CObBinTree::GetHead()
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject* CObBinTree::GetHead() const
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject*& CObBinTree::GetTail()
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject* CObBinTree::GetTail() const
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject*& CObBinTree::GetRoot()
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

_TREE_INLINE CObject* CObBinTree::GetRoot() const
{
	return m_pNodeRoot->data;
}

_TREE_INLINE POSITION CObBinTree::GetRootPosition() const
	{ return (POSITION) m_pNodeRoot; }

_TREE_INLINE CObject*& CObBinTree::GetLeft( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

_TREE_INLINE CObject* CObBinTree::GetLeft( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

_TREE_INLINE CObject*& CObBinTree::GetRight( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

_TREE_INLINE CObject* CObBinTree::GetRight( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

_TREE_INLINE CObject*& CObBinTree::GetParent( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

_TREE_INLINE CObject* CObBinTree::GetParent( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

_TREE_INLINE CObject*& CObBinTree::GetAt(POSITION position)
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

_TREE_INLINE CObject* CObBinTree::GetAt(POSITION position) const
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

_TREE_INLINE CObject* CObBinTree::GetNext(POSITION& rPosition) const // return *Position++
	{ return ((CObBinTree*)this)->GetNext( rPosition ); }

_TREE_INLINE CObject* CObBinTree::GetPrev(POSITION& rPosition) const // return *Position--
	{ return ((CObBinTree*)this)->GetPrev( rPosition ); }

_TREE_INLINE POSITION CObBinTree::Add(CObject* newElement)
{
	return Insert(newElement);
}

//-------------------------------------------------------------------------------
//							avl tree inline functions
//-------------------------------------------------------------------------------
_TREE_INLINE int CObAvlTree::GetCount() const
	{ return m_nCount; }

_TREE_INLINE BOOL CObAvlTree::IsEmpty() const
	{ return m_nCount == 0; }

_TREE_INLINE CObject*& CObAvlTree::GetHead()
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject* CObAvlTree::GetHead() const
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject*& CObAvlTree::GetTail()
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE CObject* CObAvlTree::GetTail() const
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

_TREE_INLINE POSITION CObAvlTree::GetRootPosition() const
	{ return (POSITION) m_pNodeRoot; }

_TREE_INLINE CObject*& CObAvlTree::GetRoot()
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

_TREE_INLINE CObject* CObAvlTree::GetRoot() const
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

_TREE_INLINE CObject*& CObAvlTree::GetLeft( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

_TREE_INLINE CObject* CObAvlTree::GetLeft( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

_TREE_INLINE CObject*& CObAvlTree::GetRight( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

_TREE_INLINE CObject* CObAvlTree::GetRight( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

_TREE_INLINE CObject*& CObAvlTree::GetParent( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

_TREE_INLINE CObject* CObAvlTree::GetParent( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

_TREE_INLINE CObject*& CObAvlTree::GetAt(POSITION position)
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

_TREE_INLINE CObject* CObAvlTree::GetAt(POSITION position) const
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

_TREE_INLINE CObject* CObAvlTree::GetNext(POSITION& rPosition) const // return *Position++
	{ return ((CObAvlTree*)this)->GetNext( rPosition ); }

_TREE_INLINE CObject* CObAvlTree::GetPrev(POSITION& rPosition) const // return *Position--
	{ return ((CObAvlTree*)this)->GetPrev( rPosition ); }

_TREE_INLINE POSITION CObAvlTree::Add(CObject* newElement)
{
	return Insert(newElement);
}

#endif