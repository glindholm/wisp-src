package com.neom.wisp.wisper;

/**
 * @author khunter
 *
 * 
 */
public interface SourceModuleListener
{
	public void moduleTranslated();
	public void moduleCompiled();
	public void moduleCleared();
}
