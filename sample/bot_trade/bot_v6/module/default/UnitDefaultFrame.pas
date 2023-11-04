unit UnitDefaultFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Module.SysUtils,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Memo.Types,
  FMX.Memo;

type
  TDefaultFrame = class(TFrame, IModule)
    Memo: TMemo;
  private
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.Setting;

{ TDefaultFrame }

constructor TDefaultFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TDefaultFrame.Destroy;
begin

  inherited;
end;

function TDefaultFrame.GetCaption: WideString;
begin
  Result := 'Окно сообщение по умолчанию';
end;

function TDefaultFrame.Start: Boolean;
var
  xFileName: String;
begin
  Result := True;
  Memo.Lines.Clear;
  Memo.Lines.Add('TDefaultFrame.Start');
  xFileName := TSetting.ReadString(CONFIG_FILE_NAME);
  Memo.Lines.Add(xFileName);
end;

function TDefaultFrame.Stop: Boolean;
begin
  Result := True;
  Memo.Lines.Add('TDefaultFrame.Stop');
end;

function TDefaultFrame.UpData: Boolean;
begin
  Result := True;
  Memo.Lines.Add('TDefaultFrame.UpData');
end;

initialization
  RegistrationFrame('DEFAUL',TDefaultFrame);

finalization

end.
