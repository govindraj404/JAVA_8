using ClaimService from '../../services/index';

annotate ClaimService.Claims with @( 
    UI : {
        LineItem : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            }
        ]
    }
); 