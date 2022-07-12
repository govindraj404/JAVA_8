package com.sap.ic.cmh.utils;

import org.apache.commons.lang3.StringUtils;

public class GenericUtils {

private GenericUtils() {}
	
	/**
	 * Replace escape sequences with underscore.
	 * @param str
	 * @return sanitized String
	 */
	public static String sanitize(String str) {
		if(StringUtils.isNotBlank(str)) {
			str = str.replaceAll("(\n)+", "_").replaceAll("(\t)+", "_").replaceAll("(\r)+", "_")
                    .replaceAll("(\b)+", "_");
		} else {
			str = "";
		}
		return str;
	}
}
