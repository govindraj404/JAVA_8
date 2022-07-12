package com.sap.ic.cmh.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.sap.cds.services.persistence.PersistenceService;

@SpringBootTest
public abstract class PersistenceAbstractTest {

    @Autowired
    protected PersistenceService mockDb;

}