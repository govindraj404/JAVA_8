    using ConfigurationService from '../../services/index';

    annotate ConfigurationService.Destinations with {
    destination
    @Common.Label : '{i18n>DESTINATION}'
    @UI.HiddenFilter;
    }

    annotate ConfigurationService.Destinations with @(
    Capabilities : {SearchRestrictions.Searchable : false }
    );