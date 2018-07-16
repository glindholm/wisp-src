package com.neom.util.gui;

import java.io.File;

/**
 * Title:       DirectoryChooserTreeListener
 * Description: Interface via which a DirectoryChooserTree object notifies
 *              other objects of changes in the selected directory.
 * Copyright:   Copyright (c) 2002, 2003
 * Company:     NeoMedia Technologies, Inc.
 * @author		Kevin Hunter
 * @version	1.0
 */

public interface DirectoryChooserTreeListener
{
    /**
     *  Called when the directory selection changes.
     *  @param  dir The new directory selected.  May be <code>null</code> to
     *              indicate that there is no selection, or that the
     *              "Special Root" is currently selected.
     */
    public void directorySelected(File dir);
}