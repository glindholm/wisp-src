package com.neom.wisp.wisper;

import java.util.ArrayList;
import javax.swing.JList;
import java.io.File;
import java.io.FileFilter;
import javax.swing.DefaultListModel;
import javax.swing.ListCellRenderer;
import java.util.Arrays;
import javax.swing.JPanel;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import java.awt.FlowLayout;
import java.awt.Component;
import javax.swing.Icon;
import java.awt.Container;
import java.awt.Color;
import javax.swing.UIManager;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Point;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class FileList extends JList
{
    public FileList(MainWnd parent)
    {
        super(new FileListModel());

        m_parent = parent;
        m_model = (FileListModel)getModel();

        setCellRenderer(new ListEntryRenderer());
        addMouseListener(new FileListMouseListener());
        addListSelectionListener(new FileListSelectionListener());
    }

    public void setCurrentDirectory(File dir)
    {
        m_model.removeAllElements();

        File[] theFiles = dir.listFiles(m_fileFilter);
        if (theFiles == null || theFiles.length == 0)
        {
            return;
        }

        FileListEntry[] listEntry = new FileListEntry[theFiles.length];
        int i;
        for (i = 0; i < theFiles.length; i++)
        {
            listEntry[i] = new FileListEntry(theFiles[i]);
        }

        Arrays.sort(listEntry);

        for (i = 0; i < listEntry.length; i++)
        {
            m_model.addElement(listEntry[i]);
        }

        fireSelectionChanged(null);
        fireChecksChanged(0);
    }

    public String getSelectionString()
    {
        StringBuffer buffer = new StringBuffer();
        int nCount = getCount();
        for (int i = 0; i < nCount; i++)
        {
            FileListEntry entry = getEntry(i);
            if (entry.isChecked())
            {
                buffer.append('[');
                buffer.append(entry.getWcbFileName());
                buffer.append(']');
            }
        }
        return(buffer.toString());
    }

    public void restoreSelectionString(String s)
    {
        if (s == null || s.length() == 0)
        {
            return;
        }

        StringBuffer buffer = new StringBuffer();

        int nCount = getCount();
        for (int i = 0; i < nCount; i++)
        {
            buffer.setLength(0);
            FileListEntry entry = getEntry(i);
            buffer.append('[');
            buffer.append(entry.getWcbFileName());
            buffer.append(']');
            if (s.indexOf(buffer.toString()) >= 0)
            {
                entry.setCheck(true);
            }
        }
    }

    public int getCount()
    {
        return(m_model.size());
    }

    public int getCheckedCount()
    {
        int nCount = getCount();
        int nSelected = 0;
        for (int i = 0; i < nCount; i++)
        {
            if (getEntry(i).isChecked())
            {
                nSelected++;
            }
        }

        return(nSelected);
    }

    public FileListEntry getSelectedEntry()
    {
        int nIndex = FileList.this.getSelectedIndex();
        if (nIndex < 0)
        {
            return(null);
        }
        return(getEntry(nIndex));
    }

    public FileListEntry getEntry(int n)
    {
        return((FileListEntry)m_model.elementAt(n));
    }

    public FileListEntry[] getCheckedEntries()
    {
        int nChecked = getCheckedCount();
        if (nChecked == 0)
        {
            return(null);
        }

        FileListEntry[] array = new FileListEntry[nChecked];
        int nCount = getCount();
        int n = 0;
        for (int i = 0; i < nCount; i++)
        {
            FileListEntry entry = getEntry(i);
            if (entry.isChecked())
            {
                array[n++] = entry;
            }
        }
        return(array);
    }

    public void updateAll()
    {
        m_model.updateAll();
    }

    public void checkAll()
    {
        int nCount = m_model.size();
        if (nCount == 0)
        {
            return;
        }

        for (int i = 0; i < nCount; i++)
        {
            getEntry(i).setCheck(true);
        }

        m_model.updateAll();
        fireChecksChanged(getCheckedCount());
    }

    public void checkNone()
    {
        int nCount = m_model.size();
        for (int i = 0; i < nCount; i++)
        {
            getEntry(i).setCheck(false);
        }

        m_model.updateAll();
        fireChecksChanged(getCheckedCount());
    }

    public void invertChecks()
    {
        int nCount = m_model.size();
        for (int i = 0; i < nCount; i++)
        {
            getEntry(i).invertCheck();
        }

        m_model.updateAll();
        fireChecksChanged(getCheckedCount());
    }

    public void addFileListListener(FileListListener listener)
    {
        m_listeners.add(listener);
    }

    public void removeFileListListener(FileListListener listener)
    {
        m_listeners.remove(listener);
    }

    protected void fireSelectionChanged(FileListEntry item)
    {
        Object[] listeners = m_listeners.toArray();
        for (int i = 0; i < listeners.length; i++)
        {
            FileListListener listener = (FileListListener)listeners[i];
            listener.selectionChanged(item);
        }
    }

    protected void fireChecksChanged(int nChecked)
    {
        Object[] listeners = m_listeners.toArray();
        for (int i = 0; i < listeners.length; i++)
        {
            FileListListener listener = (FileListListener)listeners[i];
            listener.checksChanged(nChecked);
        }
    }

    private static class FileListModel extends DefaultListModel
    {
        public FileListModel()
        {
        }

        public void updateAll()
        {
            int nCount = size();
            if (nCount > 0)
            {
                update(0, nCount-1);
            }
        }

        public void update(int nStart, int nEnd)
        {
            fireContentsChanged(this, nStart, nEnd);
        }
    }

    private class ListEntryRenderer extends JPanel implements ListCellRenderer
    {
        public ListEntryRenderer()
        {
            FlowLayout layout = new FlowLayout(FlowLayout.LEFT);
            layout.setVgap(0);
            setLayout(layout);

            add(m_selected);
            add(m_wispLight);
            add(m_cobLight);
            add(m_textPart);

            m_iconList[FileListEntry.NO_LIGHT] = Images.getIcon(Images.NoLight);
            m_iconList[FileListEntry.RED_LIGHT] = Images.getIcon(Images.RedLight);
            m_iconList[FileListEntry.YELLOW_LIGHT] = Images.getIcon(Images.YellowLight);
            m_iconList[FileListEntry.GREEN_LIGHT] = Images.getIcon(Images.GreenLight);

            m_selected.setIcon(Images.getIcon(Images.WhiteCheckOn));
            m_wispLight.setIcon(m_iconList[0]);
            m_cobLight.setIcon(m_iconList[0]);
            validate();

            FileList.this.m_nCheckIconWidth =
                    layout.getHgap() * 2 +
                    m_selected.getPreferredSize().width;

            m_foreSelected = UIManager.getColor("List.selectionForeground");
            m_backSelected = UIManager.getColor("List.selectionBackground");
            m_foreUnselected = UIManager.getColor("List.foreground");
            m_backUnselected = UIManager.getColor("List.background");
        }

        public Component getListCellRendererComponent(  JList list,
                                                        Object value,
                                                        int index,
                                                        boolean isSelected,
                                                        boolean cellHasFocus)
        {
            FileListEntry entry = FileList.this.getEntry(index);

            m_wispLight.setIcon(m_iconList[entry.getWispLight()]);
            m_cobLight.setIcon(m_iconList[entry.getCobolLight()]);
            m_textPart.setText(entry.getWcbFileName());

            if (isSelected)
            {
                setBackground(m_backSelected);
                m_selected.setBackground(m_backSelected);
                m_textPart.setForeground(m_foreSelected);

                if (entry.isChecked())
                {
                    m_selected.setIcon(Images.getIcon(Images.WhiteCheckOn));
                }
                else
                {
                    m_selected.setIcon(Images.getIcon(Images.WhiteCheckOff));
                }
            }
            else
            {
                setBackground(m_backUnselected);
                m_selected.setBackground(m_backUnselected);
                m_textPart.setForeground(m_foreUnselected);

                if (entry.isChecked())
                {
                    m_selected.setIcon(Images.getIcon(Images.BlackCheckOn));
                }
                else
                {
                    m_selected.setIcon(Images.getIcon(Images.BlackCheckOff));
                }
            }

            return(this);
        }

        private JLabel  m_selected = new JLabel();
        private JLabel  m_wispLight = new JLabel();
        private JLabel  m_cobLight = new JLabel();
        private JLabel  m_textPart = new JLabel();
        private Icon[]  m_iconList = new Icon[4];
        private Color   m_foreSelected;
        private Color   m_backSelected;
        private Color   m_foreUnselected;
        private Color   m_backUnselected;
    }

    private class WispFileFilter implements FileFilter
    {
        public WispFileFilter()
        {
        }

        public boolean accept(File theFile)
        {
            if (!theFile.exists() || !theFile.isFile())
            {
                return(false);
            }

            String name = theFile.getName();
            if (name.length() < 5)
            {
                return(false);
            }

            String extension = name.substring(name.length()-4).toLowerCase();
            if (".wcb".equals(extension))
            {
                return(true);
            }

            return(false);
        }
    }

    private class FileListMouseListener extends MouseAdapter
    {
        public FileListMouseListener()
        {
        }

        public void mouseClicked(MouseEvent e)
        {
            boolean bInvert = false;

            Point p = e.getPoint();
            int nIndex = FileList.this.locationToIndex(p);
            if (nIndex >= 0 && nIndex < FileList.this.getCount())
            {
                if (p.x < FileList.this.m_nCheckIconWidth)
                {
                    bInvert = true;
                }
                else if (e.getClickCount() == 2)
                {
                    bInvert = true;
                }
            }

            if (bInvert)
            {
                FileList.this.getEntry(nIndex).invertCheck();
                FileList.this.m_model.update(nIndex, nIndex);
                FileList.this.fireChecksChanged(FileList.this.getCheckedCount());
            }
        }
    }

    private class FileListSelectionListener implements ListSelectionListener
    {
        public FileListSelectionListener()
        {
        }

        public void valueChanged(ListSelectionEvent e)
        {
            if (e.getValueIsAdjusting())
            {
                return;
            }

            int nIndex = FileList.this.getSelectedIndex();
            if (nIndex < 0)
            {
                FileList.this.fireSelectionChanged(null);
            }
            else
            {
                FileList.this.fireSelectionChanged(FileList.this.getEntry(nIndex));
            }
        }
    }

    private MainWnd             m_parent;
    private FileListModel       m_model;
    private WispFileFilter      m_fileFilter = new WispFileFilter();
    private int                 m_nCheckIconWidth;
    private ArrayList           m_listeners = new ArrayList();
}