from pydriller import Repository
import csv


with open('llvmtest2-commits.csv', 'w', newline='') as csvfile:
    commitwriter = csv.writer(csvfile, quoting=csv.QUOTE_NONE)
    commitwriter.writerow(['Filename','Added_Date','Modified Date','count','Author'])
filename_set = set()
fileauthor = {}
file_dict = {}
file_dict2 = {}

for commit in Repository('https://github.com/llvm-mirror/llvm', from_commit='e6cc2d00d9d85ee967de1b58e8dc2bbf77e6174a',
                         to_commit='2c4ca6832fa6b306ee6a7010bfb80a3f2596f824').traverse_commits():
    for file in commit.modified_files:
        str_new = str(file.new_path)
        if "test\\" or "unittests\\" in str_new:

            if file.filename not in filename_set:
                filename_set.add(file.filename)

                if file.filename in file_dict:
                    file_dict[file.filename]+= commit.committer_date
                else:

                    val = str(commit.committer_date.date())
                    file_dict[file.filename] = [val]
                    fileauthor[file.filename] = {commit.committer.name}
            else:
                if file.filename in file_dict2:

                    file_dict2.get(file.filename,[]).append(str(commit.committer_date.date()))
                    fileauthor.get(file.filename,{}).add(str(commit.committer.name))
                else:
                    val=str(commit.committer_date.date())
                    file_dict2[file.filename] = [val]
                    fileauthor.get(file.filename,{}).add(str(commit.committer.name))

with open('llvmtest2-commits.csv', 'a', newline='') as csvfile:
    commitwriter = csv.writer(csvfile, quoting=csv.QUOTE_NONE, escapechar=' ')
    for key,value in file_dict.items():
        print(key)
        val = "Not modified"
        if key in file_dict2.keys():
            val=""
            for i in file_dict2[key]:
                val+=i+" "
        val1 = "No author"
        count = 0
        if key in fileauthor.keys():
            val1 = ""
            for i in fileauthor[key]:
                val1 += i+" "
                count +=1

        print("hello")
        commitwriter.writerow([key,value,val,count,val1])