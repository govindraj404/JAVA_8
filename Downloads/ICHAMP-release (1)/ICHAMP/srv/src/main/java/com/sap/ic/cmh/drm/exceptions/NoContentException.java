package com.sap.ic.cmh.drm.exceptions;

import com.sap.cds.EmptyResultException;

public class NoContentException extends Exception {

  private static final long serialVersionUID = 1L;

  public NoContentException() {
    super();
  }

  public NoContentException(String message) {
    super(message);
  }

  public NoContentException(EmptyResultException e) {
    super(e);
  }
}
