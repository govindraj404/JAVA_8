package com.sap.ic.cmh.utils;

import org.springframework.stereotype.Service;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.concurrent.*;

@Service
public class QnValidation {

    private static final Logger logger = LoggerFactory.getLogger(CommonFunctions.class);
    private ConcurrentMap<String, String>  qnMap = new ConcurrentHashMap<>();

    public ConcurrentMap<String, String> getQnMap(){
        logger.info("[QnValidation] [getQnMap]: ");
        return qnMap;
    }

    public String removeQnMap(String qnNumber){
        logger.info("[QnValidation] [removeQnMap]: ");
        return qnMap.remove(qnNumber);
    }

}
