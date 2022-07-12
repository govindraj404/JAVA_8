package com.sap.ic.cmh;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan({"com.sap.cloud.sdk", "com.sap.ic.cmh"})
@ServletComponentScan({"com.sap.cloud.sdk", "com.sap.ic.cmh"})
public class Application {
    public static void main(String[] args) {
        new SpringApplicationBuilder(Application.class).run(args);
    }
}