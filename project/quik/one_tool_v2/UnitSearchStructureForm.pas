unit UnitSearchStructureForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid;

type
  TSearchStructureForm = class(TForm)
    StrGrid: TStringGrid;
    StringColumnID: TStringColumn;
    StringColumnLengthPrice: TStringColumn;
    StringColumnLengthValue: TStringColumn;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SearchStructureForm: TSearchStructureForm;

implementation

{$R *.fmx}

{ TSearchStructureForm }

constructor TSearchStructureForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TSearchStructureForm.Destroy;
begin

  inherited;
end;

end.
