unit UnitSourceForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Lb.SysUtils.Table, Vcl.ExtCtrls;

type
  TSourceForm = class(TForm)
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSource: TSourceCandel;
    procedure SetHeardTitle;
    procedure SetSourceCandel(const Value: TSourceCandel);
  protected
    procedure SetSourceCandelShow;
  public
    property SourceCandel: TSourceCandel write SetSourceCandel;
  end;

var
  SourceForm: TSourceForm;

implementation

{$R *.dfm}

procedure TSourceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSourceForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Источник данных:';
  Self.SetHeardTitle;
end;

procedure TSourceForm.SetHeardTitle;
begin
  StrGrid.ColCount := 6;
  with StrGrid.Rows[0] do
  begin
    Clear;
    Add('Дате');
    Add('Время');
    Add('Open');
    Add('High');
    Add('Low');
    Add('Close');
  end;
end;

procedure TSourceForm.SetSourceCandel(const Value: TSourceCandel);
begin
  FSource := Value;
  var xRowCount := FSource.RowCount;
  if xRowCount > 0 then
  begin
    StrGrid.RowCount := xRowCount;
  end
  else
  begin
    StrGrid.RowCount := 2;
    StrGrid.Rows[1].Clear;
  end;
  Self.SetSourceCandelShow;
end;

procedure TSourceForm.TimerTimer(Sender: TObject);
begin
  SetSourceCandelShow;
end;

procedure TSourceForm.SetSourceCandelShow;
begin
  if Assigned(FSource) then
  begin
    var xCount := FSource.RowCount;
    if xCount > 0 then
    for var i := 1 to xCount do
    begin
      FSource.RowID := i;      
      with StrGrid.Rows[i] do
      begin
        Clear;
        Add(VarToStrDef(FSource.Values[1,i].Value,''));
        Add(VarToStrDef(FSource.Values[2,i].Value,''));
        Add(FloatToStr(FSource.Open));
        Add(FloatToStr(FSource.High));
        Add(FloatToStr(FSource.Low));
        Add(FloatToStr(FSource.Close));
      end;
    end;
  end;
end;

end.
