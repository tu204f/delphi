program import_file;

{$APPTYPE CONSOLE}

{$R *.res}

(*******************************************************************************
  Цель программы испортировать данные в файл в виде опеределеной структуры
*******************************************************************************)

uses
  System.SysUtils,
  Lb.Candel.Source in '..\src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in '..\src\Lb.Candel.SysUtils.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Candel.ImportFile in '..\src\Lb.Candel.ImportFile.pas';

const
  FILE_NAME = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\data.csv';


procedure WriteCandels(const ACandels: TCandelList);
var
  xCandel: TCandel;
begin
  for xCandel in ACandels do
    WriteCandel(xCandel);
end;

procedure WriteStructure(const AStructure: TStructure);
begin
  Writeln('SourceCount:',AStructure.SourceVectors.Count);
  WriteCandels(AStructure.SourceVectors);
  Writeln('FutureCount:',AStructure.FutureVectors.Count);
  WriteCandels(AStructure.FutureVectors);
end;

var
  StructureStream: TStructureStream;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    StructureStream := TStructureStream.Create;
    try
      StructureStream.SourceCount := 20;
      StructureStream.FutureCount := 5;
      StructureStream.FileName := FILE_NAME;

      var xIndex := 0;

      StructureStream.FirstStructure;
      //while not StructureStream.EOF do
      //begin
        Writeln('************************************************************');
        Writeln(' New Structure');
        WriteStructure(StructureStream.Structure);

    //    Inc(xIndex);
    //    if xIndex > 5 then
    //      Break;

    //    StructureStream.NextStructure;
    //  end;

    finally
      FreeAndNil(StructureStream);
    end;

    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
