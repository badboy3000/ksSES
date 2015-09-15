# ksSES
Delphi interface to Amazon SES (Simple Email Service)

Example uses...


#### Vefify a new email address
```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  ASES: IksSES;
begin
  // create the interface
  ASES := CreateSes(sesEndpointIreland, 'YOUR_PUBLIC_KEY', 'YOUR_PRIVATE_KEY');
// verify email identity...
  ASES.VerifyEmailIdentity('graham@kernow-software.co.uk');
end;
```

#### Get all verified senders
```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  ASES: IksSES;
  ASenders: TStrings;
begin
  // create the interface
  ASES := CreateSes(sesEndpointIreland, 'YOUR_PUBLIC_KEY', 'YOUR_PRIVATE_KEY');

  ASenders := TStringList.Create;
  try
    // get verified senders...
    ASES.GetSenders(ASenders, True);
    ShowMessage(ASenders.Text);
  finally
    ASenders.Free;
  end;
end;
```

#### Send an email
```pascal
procedure TForm1.Button1Click(Sender: TObject);
const
  CR = #13#10;
var
  ASES: IksSES;
begin
  // create the interface
  ASES := CreateSes(sesEndpointIreland, 'YOUR_PUBLIC_KEY', 'YOUR_PRIVATE_KEY');

  // send an email...
  ASes.SendEmail('graham@kernow-software.co.uk',
                 'person@somewhere.com',
                 'the subject',
                 'first line of the body :-)'+CR+CR+'second line of the body');
end;
```
