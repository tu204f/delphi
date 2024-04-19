unit Lb.SaveHistory;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

type
  TSaveHistory = class(TObject)
  private
    FStartTime: String;
    FSources: TStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddPrice(AStartTime, AValue: String);
    procedure Save(const AFileName: String);
    property StartTime: String read FStartTime write FStartTime;
  end;

implementation

{ TSaveHistory }

procedure TSaveHistory.Clear;
begin
  FSources.Clear;
end;

constructor TSaveHistory.Create;
begin
  FSources := TStringList.Create;
end;

destructor TSaveHistory.Destroy;
begin
  FreeAndNil(FSources);
  inherited;
end;

procedure TSaveHistory.AddPrice(AStartTime, AValue: String);
begin
  if FStartTime.IsEmpty then
    FStartTime := AStartTime;
  FSources.Add(AValue);
end;

procedure TSaveHistory.Save(const AFileName: String);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + AFileName + '.csv';
  FSources.SaveToFile(xFileName);
end;

end.
