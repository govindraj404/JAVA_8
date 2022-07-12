//package com.sap.ic.cmh;
//
//import org.junit.Before;
//import org.junit.Test;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.MockitoAnnotations;
//import org.springframework.boot.SpringApplication;
//import org.springframework.boot.builder.SpringApplicationBuilder;
//
//public class ApplicationTest {
//    @InjectMocks
//    Application  application;
//    @Mock
//    SpringApplicationBuilder springApplicationBuilder;
//    @Mock
//    SpringApplication springApplication;
//    String[] arg = null;
//
//    @Before
//    public void beforeClass() {
//        MockitoAnnotations.openMocks(this);
//    }
//
//    @Test
//    public void configureTest() {
//        application.configure(springApplicationBuilder);
//    }
//
//    @Test
//    public void testMain(){
//       // application.main(arg);
//    }
//}
