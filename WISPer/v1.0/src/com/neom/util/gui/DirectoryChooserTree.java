package com.neom.util.gui;

import java.awt.Component;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * Title:       DirectoryChooserTree
 * Description: A class to allow selection of a particular directory in the
 *              file system.
 * Copyright:   Copyright (c) 2002
 * Company:     NeoMedia Technologies, Inc.
 * @author      Kevin Hunter
 * @version     1.0
 */

public class DirectoryChooserTree extends JTree
    implements TreeWillExpandListener, TreeExpansionListener, TreeSelectionListener
{
    /**
     *  Standard constructor.
     */

    public DirectoryChooserTree()
    {
        setEditable(false);
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

        /*
         *  In a multi-rooted system like Windows, we want the root directories
         *  to be at the top of the hierarchy.  This requires using an invisible
         *  root node that is the parent of all these nodes.  We do this in
         *  single-rooted systems as well, because there seems to be some odd
         *  treatment of a visible root node that has no descendents - for
         *  some reason we get only collapse events once we close it.
         */

        DirectoryNode treeRoot;
        File[] fileSystemRoots = File.listRoots();
        Arrays.sort(fileSystemRoots);

        treeRoot = new DirectoryNode();
        for (int i = 0; i < fileSystemRoots.length; i++)
        {
            DirectoryNode dir = new DirectoryNode(fileSystemRoots[i]);
            treeRoot.add(dir);
        }
        setRootVisible(false);
        setShowsRootHandles(true);

        m_model = new DefaultTreeModel(treeRoot);
        setModel(m_model);

        addTreeSelectionListener(this);
        addTreeWillExpandListener(this);
        addTreeExpansionListener(this);
    }

    /**
     *  Sets the directory that will be initially selected.
     *  Will cause a <code>directorySelected</code> event to all
     *  registered <code>DirectoryChooserTreeListener</code>s.
     *  @param  dirInitial  The initial directory to select.  May be <code>null</code>.
     */

    public void setInitialDirectory(File dirInitial)
    {
        if (dirInitial != null)
        {
            expandNode(dirInitial.getParentFile());
            DirectoryNode node = (DirectoryNode)m_mapPathToNode.get(dirInitial.getAbsolutePath());
            TreeNode[] nodeList = m_model.getPathToRoot(node);
            TreePath path = new TreePath(nodeList);
            setSelectionPath(path);
            scrollPathToVisible(path);
        }

        fireDirectorySelected(dirInitial);
    }

    /**
     *  Ensures that the currently selected directory is visible.
     */
    public void scrollIfNecessary()
    {
        TreePath path = getSelectionPath();
        if (path != null)
        {
            scrollPathToVisible(path);
        }
    }

    /**
     *  Adds a new <code>DirectoryChooserTreeListener</code>
     *  @param  listener    Reference to the new listener
     *  @see    #removeDirectoryChooserTreeListener(DirectoryChooserTreeListener listener)
     */
    public void addDirectoryChooserTreeListener(DirectoryChooserTreeListener listener)
    {
        m_listListeners.add(listener);
    }

    /**
     *  Removes a <code>DirectoryChooserTreeListener</code> previously added
     *  using <code>addDirectoryChooserTreeListener</code>.
     *  @param  listener    The listener to remove.
     *  @see    #addDirectoryChooserTreeListener(DirectoryChooserTreeListener listener)
     */
    public void removeDirectoryChooserTreeListener(DirectoryChooserTreeListener listener)
    {
        m_listListeners.remove(listener);
    }

    /**
     *  Causes the <code>directorySelected</code> method on all the registered
     *  <code>DirectoryChooserTreeListener</code>s to be called.
     */
    protected void fireDirectorySelected(File dir)
    {
        Object[] listeners = m_listListeners.toArray();
        for (int i = 0; i < listeners.length; i++)
        {
            DirectoryChooserTreeListener listener = (DirectoryChooserTreeListener)listeners[i];
            listener.directorySelected(dir);
        }
    }

    /**
     *  <code>TreeExpansionListener</code> method called after a node has been
     *  expanded.
     *  @param  e   <code>TreeExpansionEvent</code> describing nature of the expansion.
     */
    public void treeExpanded(TreeExpansionEvent e)
    {
    }

    /**
     *  <code>TreeExpansionListener</code> method called after a node has been
     *  collapsed.  Removes the children of this node recursively so that when
     *  the node is expanded again we will re-enumerate the children.  This allows
     *  us to adapt to changes in the underlying directory structure.
     *  @param  e   <code>TreeExpansionEvent</code> describing nature of the collapse.
     */
    public void treeCollapsed(TreeExpansionEvent e)
    {
        TreePath path = e.getPath();
        if (path != null)
        {
            DirectoryNode node = (DirectoryNode)path.getLastPathComponent();
            if (node != null)
            {
                node.removeChildren();
                m_model.nodeStructureChanged(node);
            }
        }
    }

    /**
     *  <code>TreeWillExpandListener</code> method called before a node is
     *  expanded.  Causes the immediate children of the expanding node to be enumerated
     *  and added to the tree.
     *  @param  e   <code>TreeExpansionEvent</code> describing nature of the expansion.
     */
    public void treeWillExpand(TreeExpansionEvent e)
    {
        TreePath path = e.getPath();
        if (path != null)
        {
            DirectoryNode node = (DirectoryNode)path.getLastPathComponent();
            if (node != null)
            {
                node.expandChildren();
            }
        }
    }

    /**
     *  <code>TreeWillExpandListener</code> method called before a node is
     *  collapsed.
     *  @param  e   <code>TreeExpansionEvent</code> describing nature of the collapse.
     */
    public void treeWillCollapse(TreeExpansionEvent e)
    {
    }

    /**
     *  <code>TreeSelectionListener</code>method called when the selection in the
     *  tree changes.  Extracts the <code>File</code> for the selected node and
     *  uses <code>fireDirectorySelected</code> to notify the registered
     *  <code>DirectoryChooserTreeListener</code>s of the change.
     */
    public void valueChanged(TreeSelectionEvent e)
    {
        File selectedDirectory = null;
        TreePath path = e.getPath();
        if (path != null)
        {
            DirectoryNode selectedNode = (DirectoryNode)path.getLastPathComponent();
            if (selectedNode != null)
            {
                selectedDirectory = selectedNode.getDirectory();
            }
        }
        this.fireDirectorySelected(selectedDirectory);
    }

    public void setDirectorySortCaseSensitive(boolean b)
    {
        m_bSortCaseSensitive = b;
    }

    /**
     *  Expands the tree down to the specified directory.  Used to expand
     *  the tree to the initially selected directory.
     *  @param  dir     <code>File</code> object for the directory to which
     *                  the tree should expand.
     */
    private void expandNode(File dir)
    {
        if (dir == null)
        {
            return;
        }

        expandNode(dir.getParentFile());
        String path = dir.getAbsolutePath();
        DirectoryNode node = (DirectoryNode)m_mapPathToNode.get(path);
        TreeNode[] nodeList = m_model.getPathToRoot(node);
        node.expandChildren();
        this.setExpandedState(new TreePath(nodeList), true);
    }

    private DefaultTreeModel    m_model;
    private HashMap             m_mapPathToNode = new HashMap();
    private ArrayList           m_listListeners = new ArrayList();
    private boolean             m_bSortCaseSensitive = true;

    private class DirectoryNode extends DefaultMutableTreeNode implements Comparable
    {
        protected DirectoryNode()
        {
            m_strName = "";
        }

        public DirectoryNode(File dir)
        {
            super(dir);

            m_strAbsolutePath = dir.getAbsolutePath();
            m_strName = dir.getName();
            if (m_strName == null || m_strName.length() == 0)
            {
                m_strName = m_strAbsolutePath;
            }

            DirectoryChooserTree.this.m_mapPathToNode.put(m_strAbsolutePath, this);
        }

        public boolean isSpecialRoot()
        {
            return(false);
        }

        public String toString()
        {
            return(m_strName);
        }

        public File getDirectory()
        {
            return((File)getUserObject());
        }

        public String getAbsolutePath()
        {
            return(m_strAbsolutePath);
        }

        public boolean isLeaf()
        {
            return(false);
        }

        public boolean getAllowsChildren()
        {
            return(true);
        }

        public void expandChildren()
        {
            if (m_bExpanded)
            {
                return;
            }

            m_bExpanded = true;

            File me = getDirectory();
            File[] children = me.listFiles();
            if (children == null)
            {
                return;
            }
            int i;
            ArrayList list = new ArrayList();
            for (i = 0; i < children.length; i++)
            {
                if (children[i].isDirectory())
                {
                    DirectoryNode childNode = new DirectoryNode(children[i]);
                    list.add(childNode);
                }
            }
            Object[] array = list.toArray();
            Arrays.sort(array);

            for (i = 0; i < array.length; i++)
            {
                add((DirectoryNode)array[i]);
            }

            if (DirectoryChooserTree.this.m_model != null)
            {
                DirectoryChooserTree.this.m_model.nodeStructureChanged(this);
            }
        }

        public void removeChildren()
        {
            Enumeration children = children();
            while(children.hasMoreElements())
            {
                DirectoryNode child = (DirectoryNode)children.nextElement();
                child.removeChildren();
                DirectoryChooserTree.this.m_mapPathToNode.remove(child.m_strAbsolutePath);
            }

            removeAllChildren();
            m_bExpanded = false;
        }

        public int compareTo(Object o)
        {
            String me = toString();
            String other = o.toString();
            if (me == null)
            {
                me = "";
            }
            if (other == null)
            {
                other = "";
            }
            if (!DirectoryChooserTree.this.m_bSortCaseSensitive)
            {
                me = me.toLowerCase();
                other = other.toLowerCase();
            }
            return(me.compareTo(other));
        }

        private String  m_strAbsolutePath;
        private String  m_strName;
        private boolean m_bExpanded = false;
    }

}