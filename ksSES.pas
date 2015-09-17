{*******************************************************************************
*                                                                              *
*  ksSES - AWS SES Interface                                                   *
*                                                                              *
*  https://github.com/gmurt/ksSES                                              *
*                                                                              *
*  Copyright 2015 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}


unit ksSES;

interface

uses
  Sysutils, Classes, System.Net.HttpClientComponent,
  System.Net.HttpClient;

const
  C_ENDPOINT_OREGON          = 'email.us-west-2.amazonaws.com';
  C_ENDPOINT_NORTH_VIRGINIA  = 'email.us-east-1.amazonaws.com';
  C_ENDPOINT_IRELAND         = 'email.eu-west-1.amazonaws.com';

type
  TksSESEndpoint = (sesEndpointOregon, sesEndpointNorthVirginia, sesEndpointIreland);

  TksSESSendQuota = record
    Max24HourSend: Extended;
    MaxSendRate: Extended;
    SentLast24Hours: Extended;
  end;

 IksSesMessageDestination = interface
  ['{E72991E5-8C97-46B3-84BA-39E497FF86D7}']
    function GetBccList: TStrings;
    function GetCcList: TStrings;
    function GetReipients: TStrings;
    property BccList: TStrings read GetBccList;
    property CcList: TStrings read GetCcList;
    property Recipients: TStrings read GetReipients;
  end;

  IksSesMessage = interface
  ['{21287B27-35CE-473E-8AB0-88809492E72F}']
    function GetBody: string;
    function GetSubject: string;
    procedure SetBody(const Value: string);
    procedure SetSubject(const Value: string);
    property Body: string read GetBody write SetBody;
    property Subject: string read GetSubject write SetSubject;
  end;

  IksSES = interface
  ['{79EE7247-AF52-40C8-B230-321B4AF06F2C}']
    function GetEndpoint: TksSESEndpoint;
    procedure SetEndpoint(Value: TksSESEndpoint);
    function IsEmailVerified(AEmail: string): Boolean;
    function GetSendQuota: TksSESSendQuota;
    procedure DeleteIdentity(AIdentity: string);
    procedure GetSenders(ASenders: TStrings; AVerifiedOnly: Boolean);
    procedure VerifyEmailIdentity(AEmail: string);
    procedure SendEmail(AFrom, ATo, ASubject, ABody: string); overload;
    procedure SendEmail(AFrom: string;
                        ADestination: IksSesMessageDestination;
                        AMessage: IksSesMessage); overload;
    property Endpoint: TksSESEndpoint read GetEndpoint write SetEndpoint;
  end;

  function CreateSes(AEndpoint: TksSESEndpoint; APublicKey, APrivateKey: string): IksSES;
  function CreateSesMessage: IksSesMessage;
  function CreateSesMessageDestination: IksSesMessageDestination;


implementation

uses DateUtils, ComObj, XMLIntf, XMLDoc, synacode;

type
  TksSesMessageDestination = class(TInterfacedObject, IksSesMessageDestination)
  private
    FBccList: TStrings;
    FCcList: TStrings;
    FRecipients: TStrings;
    function GetBccList: TStrings;
    function GetCcList: TStrings;
    function GetReipients: TStrings;
  protected
    property BccList: TStrings read GetBccList;
    property CcList: TStrings read GetCcList;
    property Recipients: TStrings read GetReipients;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TksSesMessage = class(TInterfacedObject, IksSesMessage)
  private
    FBody: string;
    FSubject: string;
    function GetBody: string;
    function GetSubject: string;
    procedure SetBody(const Value: string);
    procedure SetSubject(const Value: string);
  protected
    property Body: string read GetBody write SetBody;
    property Subject: string read GetSubject write SetSubject;
  end;


  TksSES = class(TInterfacedObject, IksSes)
  private
    { Private declarations }
    FPublickey: string;
    FPrivateKey: string;
    FHttp: TNetHTTPClient;
    FEndpoint: TksSESEndpoint;
    FParams: TStrings;
    function GetEndpoint: TksSESEndpoint;
    procedure SetEndpoint(Value: TksSESEndpoint);
    function GetEndpointStr: string;
    function ExecuteCommand(ACmd: string; AParams: TStrings): string;
    function GetCurrentDate(ADate: TDateTime): string;
    procedure ListVerifiedEmailAddresses(ASenders: TStrings);
    procedure ListIdentities(AIdentities: TStrings);
  protected
    function GetSendQuota: TksSESSendQuota;
    function IsEmailVerified(AEmail: string): Boolean;
    procedure DeleteIdentity(AIdentity: string);
    procedure GetSenders(ASenders: TStrings; AVerifiedOnly: Boolean);
    procedure VerifyEmailIdentity(AEmail: string);
    procedure SendEmail(AFrom, ATo, ASubject, ABody: string); overload;
    procedure SendEmail(AFrom: string;
                        ADestination: IksSesMessageDestination;
                        AMessage: IksSesMessage); overload;
    property Endpoint: TksSESEndpoint read GetEndpoint write SetEndpoint;
  public
    constructor Create(AEndpoint: TksSESEndpoint; APublicKey, APrivateKey: string);
    destructor Destroy; override;
    { Public declarations }
  end;




function CreateSes(AEndpoint: TksSESEndpoint; APublicKey, APrivateKey: string): IksSES;
begin
  Result := TksSES.Create(AEndpoint, APublicKey, APrivateKey);
end;

function CreateSesMessage: IksSesMessage;
begin
  Result := TksSesMessage.Create;
end;

function CreateSesMessageDestination: IksSesMessageDestination;
begin
  Result := TksSesMessageDestination.Create;
end;

constructor TksSES.Create(AEndpoint: TksSESEndpoint; APublicKey, APrivateKey: string);
begin
  FParams := TStringList.Create;
  FpublicKey := APublicKey;
  FprivateKey := APrivateKey;
  FEndpoint := AEndpoint;
end;

procedure TksSES.DeleteIdentity(AIdentity: string);
var
  AParams: TStringList;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Identity'] := AIdentity;
    ExecuteCommand('DeleteIdentity', AParams);
  finally
    AParams.Free;
  end;
end;

destructor TksSES.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TksSES.GetCurrentDate(ADate: TDateTime): string;
const
  FORMAT_HTTP_DATE = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';
begin
  Result := FormatDateTime(FORMAT_HTTP_DATE, TTimeZone.Local.ToUniversalTime(ADate), TFormatSettings.Create('en-US'));
end;

function TksSES.ExecuteCommand(ACmd: string; AParams: TStrings): string;
var
  AResponse: IHTTPResponse;
  ADate: string;
  ASig: AnsiString;
  ICount: integer;
  AStr: string;
begin
  FHttp := TNetHTTPClient.Create(nil);
  FParams.Clear;
  if AParams <> nil then
    FParams.Assign(AParams);
  FParams.Insert(0, 'Action='+ACmd);
  try
    ADate := GetCurrentDate(now);
    ASig := EncodeBase64(HMAC_SHA1(AnsiString(ADate), AnsiString(FPrivateKey)));
    FHttp.CustomHeaders['Date'] := ADate;
    FHttp.CustomHeaders['Host'] := GetEndpointStr;
    FHttp.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded';
    FHttp.CustomHeaders['X-Amzn-Authorization'] := 'AWS3-HTTPS AWSAccessKeyId='+FPublickey+', Algorithm=HmacSHA1, Signature='+string(ASig);
    AStr := '';
    for ICount := 0 to FParams.Count-1 do
    begin
      AStr := AStr + FParams[ICount];
      if ICount < FParams.Count-1 then
        AStr := AStr + '&';
    end;
    AResponse := FHttp.Post('https://'+GetEndpointStr, FParams);
    Result := AResponse.ContentAsString;
  finally
    FHttp.Free;
  end;
end;

function TksSES.GetEndpoint: TksSESEndpoint;
begin
  Result := FEndpoint;
end;

function TksSES.GetEndpointStr: string;
begin
  case FEndpoint of
    sesEndpointOregon         : Result := C_ENDPOINT_OREGON;
    sesEndpointNorthVirginia  : Result := C_ENDPOINT_NORTH_VIRGINIA;
    sesEndpointIreland        : Result := C_ENDPOINT_IRELAND;
  end;
end;

procedure TksSES.ListVerifiedEmailAddresses(ASenders: TStrings);
var
  AXml: IXMLDocument;
  AResult: string;
  ARoot: IXMLNode;
  ANode: IXMLNode;
  ICount: integer;
begin
  ASenders.Clear;
  AXml := TXMLDocument.Create(nil);
  AResult := ExecuteCommand('ListVerifiedEmailAddresses', nil);
  AXml.LoadFromXML(AResult);
  ARoot := AXml.DocumentElement;
  ANode := ARoot.ChildNodes['ListVerifiedEmailAddressesResult'];
  ANode := ANode.ChildNodes['VerifiedEmailAddresses'];
  for iCount := 0 to ANode.ChildNodes.Count -1 do
    ASenders.Add(LowerCase(ANode.ChildNodes[ICount].Text));
end;

procedure TksSES.ListIdentities(AIdentities: TStrings);
var
  AXml: IXMLDocument;
  AResult: string;
  ARoot: IXMLNode;
  ANode: IXMLNode;
  ICount: integer;
begin
  AIdentities.Clear;
  AXml := TXMLDocument.Create(nil);
  AResult := ExecuteCommand('ListIdentities', nil);
  AXml.LoadFromXML(AResult);
  ARoot := AXml.DocumentElement;
  ANode := ARoot.ChildNodes['ListIdentitiesResult'];
  ANode := ANode.ChildNodes['Identities'];
  for iCount := 0 to ANode.ChildNodes.Count -1 do
    AIdentities.Add(LowerCase(ANode.ChildNodes[ICount].Text));
end;

procedure TksSES.GetSenders(ASenders: TStrings; AVerifiedOnly: Boolean);
begin
  case AVerifiedOnly of
    True: ListVerifiedEmailAddresses(ASenders);
    False: ListIdentities(ASenders);
  end;
end;

function TksSES.GetSendQuota: TksSESSendQuota;
var
  AXml: IXMLDocument;
  AResult: string;
  ANode: IXMLNode;
begin
  AXml := TXMLDocument.Create(nil);
  AResult := ExecuteCommand('GetSendQuota', nil);
  AXml.LoadFromXML(AResult);
  ANode := AXml.DocumentElement.ChildNodes['GetSendQuotaResult'];
  Result.Max24HourSend := StrToFloat(ANode.ChildNodes['Max24HourSend'].Text);
  Result.MaxSendRate := StrToFloat(ANode.ChildNodes['MaxSendRate'].Text);
  Result.SentLast24Hours := StrToFloat(ANode.ChildNodes['SentLast24Hours'].Text);
end;

function TksSES.IsEmailVerified(AEmail: string): Boolean;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    GetSenders(AStrings, True);
    Result := AStrings.IndexOf(LowerCase(AEmail)) > -1;
  finally
    AStrings.Free;
  end;
end;

procedure TksSES.SendEmail(AFrom, ATo, ASubject, ABody: string);
var
  AMessage: IksSesMessage;
  ADestination: IksSesMessageDestination;
begin
  AMessage := CreateSesMessage;
  ADestination := CreateSesMessageDestination;

  ADestination.Recipients.Add(ATo);
  AMessage.Subject := ASubject;
  AMessage.Body := ABody;
  SendEmail(AFrom, ADestination, AMessage);
end;

procedure TksSES.SendEmail(AFrom: string;
                           ADestination: IksSesMessageDestination;
                           AMessage: IksSesMessage);

var
  AParams: TStrings;
  ICount: integer;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Source'] := AFrom;
    for ICount := 1 to ADestination.Recipients.Count do
      AParams.Values['Destination.ToAddresses.member.'+IntToStr(ICount)] := ADestination.Recipients[ICount-1];
    for ICount := 1 to ADestination.CcList.Count do
      AParams.Values['Destination.CcAddresses.member.'+IntToStr(ICount)] := ADestination.CcList[ICount-1];
    for ICount := 1 to ADestination.BccList.Count do
      AParams.Values['Destination.BccAddresses.member.'+IntToStr(ICount)] := ADestination.BccList[ICount-1];
    AParams.Values['Message.Subject.Data'] := AMessage.Subject;
    AParams.Values['Message.Body.Text.Data'] := AMessage.Body;
    ExecuteCommand('SendEmail', AParams);
  finally
    AParams.Free;
  end;
end;

procedure TksSES.SetEndpoint(Value: TksSESEndpoint);
begin
  FEndpoint := Value;
end;

procedure TksSES.VerifyEmailIdentity(AEmail: string);
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['EmailAddress'] := AEmail;
    ExecuteCommand('VerifyEmailIdentity', AParams);
  finally
    AParams.Free;
  end;
end;

{ TksSesMessage }

function TksSesMessage.GetBody: string;
begin
  Result := FBody;
end;

function TksSesMessage.GetSubject: string;
begin
  Result := FSubject;
end;

procedure TksSesMessage.SetBody(const Value: string);
begin
  FBody := Value;
end;

procedure TksSesMessage.SetSubject(const Value: string);
begin
  FSubject := Value;
end;

{ TksSesMessageDestination }

constructor TksSesMessageDestination.Create;
begin
  FBccList := TStringList.Create;
  FCcList := TStringList.Create;
  FRecipients := TStringList.Create;
end;

destructor TksSesMessageDestination.Destroy;
begin
  FBccList.Free;
  FCcList.Free;
  FRecipients.Free;
  inherited;
end;

function TksSesMessageDestination.GetBccList: TStrings;
begin
  Result := FBccList;
end;

function TksSesMessageDestination.GetCcList: TStrings;
begin
  Result := FCcList;
end;

function TksSesMessageDestination.GetReipients: TStrings;
begin
  Result := FRecipients;
end;

end.


