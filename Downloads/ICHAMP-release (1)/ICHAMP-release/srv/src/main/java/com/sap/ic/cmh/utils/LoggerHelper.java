package com.sap.ic.cmh.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerHelper {
	public static final String LOG_ERRROR = "Error";
	public static final String LOG_INFO = "Info";
	public static final String LOG_TRACE = "Trace";
	public static final String LOG_DEBUG = "Debug";
	
	/**
	 * default constructor
	 */
	private LoggerHelper() {
		//default constructor
	}

	public static Logger getLogger(Class<?> className) {
		return LoggerFactory.getLogger(className);
	}
	
	public static void logConstructorEntry(Logger logger, String className) {
		logger.trace("Class '{}' instance instantiation started", className);
	}

	public static void logConstructorExit(Logger logger, String className) {
		logger.trace("Class '{}' instance instantiated", className);
	}

	public static void logMethodEntry(Logger logger, String className, String methodName) {
		logger.trace("Class '{}' method '{}' invoked", className, methodName);
	}

	public static void logMethodExit(Logger logger, String className, String methodName) {
		logger.trace("Class '{}' method '{}' ended", className, methodName);
	}

	public static void logExceptionWithMessage(Logger logger, String message, Exception e) {
		if (message == null || message.isEmpty()) {
			message = "Exception occurred";
		}
		String logMessage = message + " - '{}' - '{}'";
		logMessage =encodeUserInputForLog(logMessage);
		logger.error(logMessage, e.getClass().getName(), e.getMessage());
		logger.trace(logMessage, e.getClass().getName(), e.getMessage(), validateExceptionLength(e));
		logger.debug(logMessage, e.getClass().getName(), e.getMessage(),validateExceptionLength(e));
	}
	
	private static String encodeUserInputForLog(String input) {
		if (input != null) {
			input = input.replace("\n", "\\\\n");
		}
		return input;
	}

	private static String validateUserInputForLog(String input, int len) {
		if (input != null) {
			input = encodeUserInputForLog(input);
			if (input.length() > len) {
				input = input.substring(0, len);
			}
		}
		return input;
	}	
	
	public static String validateLogLength(String input){
		if (input!=null)
		{
			input = validateUserInputForLog(input,1024);
		}
		return input;
	}
	
	public static String validateExceptionLength(Exception e) {
		String exception = null;
		if (e != null) {
			exception = e.toString();
			exception = validateUserInputForLog(exception, 1024);
		}
		return exception;
	}
}
