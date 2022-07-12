package com.sap.ic.cmh.utils;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;

public class LoggerHelperTest {
    @InjectMocks
    LoggerHelper loggerHelper;
    @Mock
    Logger logger;
    Object[] obj=new Object[]{"test","log"};

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    // @Test
    // public void getLoggerTest(){
    //     loggerHelper.getLogger(LoggerHelperTest.class);
    // }
    // @Test
    // public void getLogTest(){
    //     loggerHelper.log(logger,"log",obj);
    // }
    // @Test
    // public void logErrorTest(){
    //     loggerHelper.log("LOG_ERRROR",logger,"log",obj);
    // }
    // @Test
    // public void logInfoTest(){
    //     loggerHelper.log("LOG_INFO",logger,"log",obj);
    // }
    // @Test
    // public void logDebugTest(){
    //     loggerHelper.log("LOG_DEBUG",logger,"log",obj);
    // }
    // @Test
    // public void logTraceTest(){
    //     loggerHelper.log("LOG_TRACE",logger,"log",obj);
    // }
    // @Test
    // public void logDraftTest(){
    //     loggerHelper.log("",logger,"log",obj);
    // }

    // @Test
    // public void logErrorWithFormatTest(){
    //     loggerHelper.log("LOG_ERRROR",logger,"log");
    // }
    // @Test
    // public void logInfoWithFormatTest(){
    //     loggerHelper.log("LOG_INFO",logger,"log");
    // }
    // @Test
    // public void logDebugWithFormatTest(){
    //     loggerHelper.log("LOG_DEBUG",logger,"log");
    // }
    // @Test
    // public void logTraceWithFormatTest(){
    //     loggerHelper.log("LOG_TRACE",logger,"log");
    // }
    // @Test
    // public void logDraftWithFormatTest(){
    //     loggerHelper.log("",logger,"log");
    // }

    // @Test
    // public void getLogErrorTest(){
    //     loggerHelper.logError(logger,"log");
    // }
    // @Test
    // public void getLogInfoTest(){
    //     loggerHelper.logInfo(logger,"log");
    // }
    // @Test
    // public void getLogDebugTest(){
    //     loggerHelper.logDebug(logger,"log");
    // }
    @Test
    public void logConstructorEntryTest(){
        loggerHelper.logConstructorEntry(logger,"log");
    }
    @Test
    public void logConstructorExitTest(){
        loggerHelper.logConstructorExit(logger,"log");
    }
    @Test
    public void logMethodEntryTest(){
        loggerHelper.logMethodEntry(logger,"log","test");
    }
    @Test
    public void logMethodExitTest(){
        loggerHelper.logMethodExit(logger,"log","test");
    }
    @Test
    public void logExceptionWithMessageTest(){
        loggerHelper.logExceptionWithMessage(logger,"log",new Exception("test"));
    }

    @Test
    public void logExceptionWithMessageNullTest(){
        loggerHelper.logExceptionWithMessage(logger," ",new Exception("test"));
    }
    @Test
    public void validateLogLengthTest(){
        loggerHelper.validateLogLength(" test");
    }@Test
    public void validateLogLengthNullTest(){
        loggerHelper.validateLogLength("");
    }
    @Test
    public void validateExceptionLengthTest(){
        loggerHelper.validateExceptionLength(new Exception());
    }
    @Test
    public void validateExceptionLengthNullTest(){
        loggerHelper.validateExceptionLength(null);
    }

    @Test
    public void logExceptionWithMessageNull2Test(){
        loggerHelper.logExceptionWithMessage(logger,null,new Exception("test"));
    }
    @Test
    public void logExceptionWithMessageEmptyTest(){
        loggerHelper.logExceptionWithMessage(logger,"",new Exception("test"));
    }
}
