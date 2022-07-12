package com.sap.ic.cmh.pdm;

public class PDMException extends Exception  {

    private static final long serialVersionUID = 1L;

    public PDMException(final String message, final Throwable inner) {
        super(message, inner);
     
    }
}