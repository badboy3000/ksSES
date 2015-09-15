# ksSES
Delphi interface to Amazon SES (Simple Email Service)

Example use...

```pascal
procedure TForm1.Button1Click(Sender: TObject);
const
  CR = #13#10;
var
  ASES: IksSES;
  ASenders: TStrings;
begin
  // create the interface
  ASES := CreateSes(sesEndpointIreland, 'YOUR_PUBLIC_KEY', 'YOUR_PRIVATE_KEY');

  // verify a new email...
  ASES.VerifyEmailIdentity('graham@kernow-software.co.uk');

  // list verified senders...
  ASenders := TStringList.Create;
  try
    ASES.GetSenders(ASenders, False);
    ShowMessage(ASenders.Text);
  finally
    ASenders.Free;
  end;
  
  // send an email...
  ASes.SendEmail('graham@kernow-software.co.uk',
                 'person@somewhere.com',
                 'the subject',
                 'first line of the body :-)'+CR+CR+'second line of the body');
end;
```
