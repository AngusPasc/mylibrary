unit TMBAuthor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBAuthor = Class
      private
             FAuthorName : String;
             FAuthorSurname : String
             FAurhorID : Integer;
             FAuthorOrigID : Integer;
             FAuthorCompositions : array of Integer;
             FAuthorBooks : areay of Integer;
             function GetAuthorCompositionID(Index : Integer) : Integer;
             function GetAuthorBookID(Index : Integer) : Integer;
             procedure GetAuthorBooks(const SQLQuery :  TSQLQuery);
             procedure GetAuthorCompositions(const SQLQuery :  TSQLQuery);
      published

      public
             constructor Create(Name : String = ""; Surname : String = "");
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             property AuthorName : String read FAuthorName write SetAuthorName;
             property AuthorSurname : String read FAuthorSurname write SetAuthorSurname;
             property AuthorID : String read FAuthorID;
             property AuthorOrigID : String read FAuthorOrigID;
             property AuthorCompositions[Index : Integer]: FAuthorCompositions read GetAuthorCompositionID;
             property AuthorBooks[Index : Integer]: FAuthorBooks read GetAuthorBookID;
             procedure UpdateAuthor();

    end;

implementation
              constructor TMBAuthor.Create(Name : String = ""; Surname : String = "");
              begin
                   //ДК, надо добавить пару проверок Name и Surname не должны быть пустыми
                   FAuthorName:=Name;
                   FAthorSurname:=Surname;
                   FAuthorID:=0;
                   FAuthorOrigID:=0;
              end;

              constructor TMBAuthor.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
              begin
                   SQLQuery.Close;
                   SQLQuery.SQL.Text:='SELECT name, surname, originid FROM authors where id=:bID';
                   SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
                   SQLQuery.Open;
                   //ДК, нужна проверка что запрос вернул не пустой результат
                   FAuthorName:=SQLQuery.FieldByName('name').AsString;
                   FAuthorSurname:=SQLQuery.FieldByName('surname').AsString;
                   FAuthorID:=ID;
                   FAuthorOriginID:=StrToInt(SQLQuery.FieldByName('originid').AsString);
                   SQLQuery1.Close;
              end;

              procedure TMBAuthor.GetAuthorCompositions(const SQLQuery :  TSQLQuery);
              var
                I : Integer;
              begin
                   SQLQuery.Close;
                   SQLQuery.SQL.Text:='SELECT composition_id FROM rel_composition_authors where author_id=:bID';
                   SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
                   SQLQuery.Open;
                   SQLQuery.Last;
                   if SQLQuery.RecordCount > 0 then
                   begin
                       SetLength(FAuthorCompositions, SQLQuery.RecordCount);
                       SQLQuery.First;
                       I:=0;
                       while not SQLQuery.EOF do
                       begin
                            FAthorCompositions[I]:=SQLQuery.FieldByName('composition_id');
                            I:=I+1;
                            SQLQuery.Next;
                       end;
                            SQLQuery.Close;
                   end;
              end;

              procedure TMBAuthor.GetAuthorBooks(const SQLQuery :  TSQLQuery);
              begin
              var
                I : Integer;
                CompositionList : String;
              begin
                   if lenght(FAuthorCompositions) > 0 then
                   begin
                        for I:=0 to lenght(FAuthorCompositions)-1 do
                        begin
                             CompositionList:=CompositionList+IntToStr(FAuthorCompositions);
                             if I < lenght(FAuthorCompositions)-1 then CompositionList:=CompositionList+',';
                        end;
                   end;
                   SQLQuery.Close;
                   SQLQuery.SQL.Text:='SELECT book_id FROM rel_book_compositions where composition_id in=(:bComposidionsID)';
                   SQLQuery.Params.ParamByName('bCompositionsID').AsString:=CompositionList;
                   SQLQuery.Open;
                   SQLQuery.Last;
                   if SQLQuery.RecordCount > 0 then
                   begin
                       SetLength(FAuthorBooks, SQLQuery.RecordCount);
                       SQLQuery.First;
                       I:=0;
                       while not SQLQuery.EOF do
                       begin
                            FAthorBooks[I]:=SQLQuery.FieldByName('book_id');
                            I:=I+1;
                            SQLQuery.Next;
                       end;
                            SQLQuery.Close;
                   end;
              end;
end.

