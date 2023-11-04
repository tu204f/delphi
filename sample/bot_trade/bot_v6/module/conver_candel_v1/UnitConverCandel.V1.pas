unit UnitConverCandel.V1;

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
  Lb.SysUtils.Candel, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, FMX.EditBox, FMX.NumberBox, Lb.SysUtils.Structure,
  FMX.Layouts;

type
  TConverCandelV1Frame = class(TFrame, IModule)
    Memo1: TMemo;
    Layout1: TLayout;
    Edit1: TEdit;
    Edit2: TEdit;
  private
    FMemoryCandels: TMemoryCandels;
    FStructures: TGenerationStructures;
    procedure EventStructurePacked(const AStructure: TStructure);
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
    property MemoryCandels: TMemoryCandels read FMemoryCandels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.Setting;

var
  IndexStructure: Integer = 0;
  IndexPositiv: Integer = 0;
  IndexNigativ: Integer = 0;

{ TConverCandelV1Frame }

constructor TConverCandelV1Frame.Create(AOwner: TComponent);
begin
  inherited;
  FMemoryCandels := TMemoryCandels.Create;
  FStructures := TGenerationStructures.Create;
  FStructures.OnStructurePacked := EventStructurePacked;
end;

destructor TConverCandelV1Frame.Destroy;
begin
  FreeAndNil(FStructures);
  FreeAndNil(FMemoryCandels);
  inherited;
end;


function TConverCandelV1Frame.GetCaption: WideString;
begin
  Result := 'Конвиртировать свячи';
end;

function TConverCandelV1Frame.Start: Boolean;
begin
  IndexStructure := 0;
  IndexPositiv := 0;
  IndexNigativ := 0;

  Result := True;
  FMemoryCandels.FileName := TSetting.ReadString(CONFIG_FILE_NAME);
  FMemoryCandels.CandelFirst;

  FStructures.SourceCount := StrToIntDef(Edit1.Text,0);
  FStructures.VectorCount := StrToIntDef(Edit2.Text,0);

  if (FStructures.SourceCount = 0) or (FStructures.VectorCount = 0) then
  begin
    Result := False;
    Exit;
  end;
end;

function TConverCandelV1Frame.Stop: Boolean;
begin
  Result := True;
end;

procedure TConverCandelV1Frame.EventStructurePacked(const AStructure: TStructure);
var
  xS: String;
  xV: Integer;
begin
  // Событие получение структура данных

  Inc(IndexStructure);

  // Возрошаем закончаниую структура
  Memo1.Lines.Clear;

  for var xCandel in AStructure.Sources do
    Memo1.Lines.Add(xCandel.ToString);

  if TStructureAPI.IsSimpleMovingAverages(AStructure,AStructure.VectorCount) then
    Inc(IndexPositiv)
  else
    Inc(IndexNigativ);

  xV := Trunc((100 * IndexPositiv)/IndexStructure);
  xS := 'IndexStructure = ' + IndexStructure.ToString +
        '; IndexPositiv = ' + IndexPositiv.ToString + '; value = ' + xV.ToString + '%' +
        '; IndexNigativ = ' + IndexNigativ.ToString;

  Memo1.Lines.Add(xS);
end;

function TConverCandelV1Frame.UpData: Boolean;
var
  xNext: Boolean;
  xCandel: TCandel;
begin
  xNext :=  not MemoryCandels.CandelEOF;
  if xNext then
  begin
    xCandel := MemoryCandels.Candel;
    FStructures.AddCandel(xCandel);
    MemoryCandels.CandelNext;
  end;
  Result := xNext;
end;

initialization
  RegistrationFrame('CONVER_CANDEL_V1',TConverCandelV1Frame);

finalization

end.
