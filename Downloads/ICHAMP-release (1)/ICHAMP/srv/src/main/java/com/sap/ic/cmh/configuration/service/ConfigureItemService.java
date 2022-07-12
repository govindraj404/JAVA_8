package com.sap.ic.cmh.configuration.service;

import java.util.List;

import cds.gen.configurationservice.ConfigureItems;

public interface ConfigureItemService {

	public List<ConfigureItems> getAllConfiguredApplications();

}
