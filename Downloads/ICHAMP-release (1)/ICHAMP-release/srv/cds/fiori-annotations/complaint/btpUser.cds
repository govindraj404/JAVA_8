using ComplaintService from '../../services/index';

annotate ComplaintService.BTPUsers with {

    personResponsibleNumber
    @Common.Label  : '{i18n>BUSINESS_PARTNER_NUMBER}';

    personResponsible_ID
    @Common.Label  : '{i18n>EMAIL}';
}