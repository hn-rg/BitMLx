import prettytable as pt
import re
import os

def find_max_num(num_list):
    """ Find the maximum number from a list of strings (transaction lines with number).

    Args:
        num_list(list): A list of strings, where each string contrains number.

    Returns:
        int: The maximum number among all strings in num_list.
    """
    trans_map = {item: int(re.findall(r"\d+", item)[0]) for item in num_list}
    max_num = max(trans_map.values())
    return max_num


def read_balzac_stats(fpath):
    """
    Read a .balzac file and find the maximum number among all transaction lines.

    Args:
        fpath (str): The path to the .balzac file containing required statistics (total
          number of transacrions).

    Returns:
        int: The maximum number among all transaction lines in the file.
    
    Raises:
        FileNotFoundError: If the file is not found.   
    """
    temp_list = []
    try:
        with open(fpath, "r") as file:
            lines = file.read().splitlines()

            for l in lines:
                txs_pattern = r"^transaction T\d+ \{ $"
                if re.match(txs_pattern, l):
                    temp_list.append(l)
    except FileNotFoundError:
        print(f"Error: File '{fpath}' not found.")

    result = find_max_num(temp_list)
    return result


def read_txt_stats(fpath):
    """
    Read a .txt file and find the maximum number in file.

    Args:
        fpath (str): The path to the .txt file containing required statistics (depth
            of transaction tree).

    Returns:
        int: The maximum number in file.
    
    Raises:
        FileNotFoundError: If the file is not found.       
    """   
    result = ""
    try:
        with open(fpath, "r") as file:
            lines = file.read().splitlines()
            result = re.findall(r"\d+", lines[0])[0]         
    except FileNotFoundError:
        print(f"Error: File '{fpath}' not found.")
    return result


def read_rkt_stats(fpath):
    """
    Read a .rkt file and find the maximum number among all transaction lines.

    Args:
        fpath (str): The path to the .balzac file containing required statistics (time
            to execute).

    Returns:
        int: The maximum number among all transaction lines in the file.
    
    Raises:
        FileNotFoundError: If the file is not found.
    """
    temp_list = []
    try:
        with open(fpath, "r") as file:
            lines = file.read().splitlines()
            for l in lines:
                timeout_pattern = r"\(after \d+ \(reveal \("
                if re.search(timeout_pattern, l):
                    temp_list.append(l)             
    except FileNotFoundError:
        print(f"Error: File '{fpath}' not found.")
    result = find_max_num(temp_list)
    return result


def gen_stats(dpath, flist):
    """ Generate a statistics summary based on statistics of all files in ./output.

    Args:
        dpath(str): The directory path (./output) containing the files with required statistics.
        flist(List of str): A list of filename. The files contain the required statistics.
    
    Returns:
        dict: A dictionary containing statistics summary for each type of required data. 
            The keys are the BitMLx contract names and the values are dictionaries containing
            statistics for each contract.
    """
    stats_summary = {}
    flist.sort()
    for fname in flist:
        fpath = os.path.join(dpath, fname)
        cname_temp = os.path.splitext(fname)[0]
        cname = cname_temp.split("_")[0]
        stats_summary.setdefault(cname, {})

        if fname.endswith("balzac"):
            f_txs = read_balzac_stats(fpath)
            stats_summary[cname]["txs"] = f_txs  
        
        if fname.endswith("txt"):
            f_depth = read_txt_stats(fpath)
            stats_summary[cname]["depth"] = f_depth

        if fname.endswith("rkt"):
            f_timeout = read_rkt_stats(fpath)
            stats_summary[cname]["timeout"] = f_timeout
    return stats_summary


def gen_table(stats):
    """
    Generate a formatted table from given statistics data.

    Args:
        stats (dict): A dictionary containing statistics data for each contract.

    Returns:
        PrettyTable: A formatted table displaying contract names with their corresponding statistics 
        (total number of transactions, depth of transaction tree and time to execute).
    """
    table = pt.PrettyTable()
    table.field_names = ["Contract name", "Total Number of Transactions", "Depth of Transaction Tree", "Time to execute"]
    for cname, data_dict in stats.items():
        txs = data_dict.get("txs", "-")
        tree_depth = data_dict.get("depth", "-")
        timeout = data_dict.get("timeout", "-")
        new_row = [cname, txs, tree_depth, timeout]
        table.add_row(new_row)
    return table


""" Main module """
if __name__ == "__main__":
    dpath = "./output"
    flist = os.listdir(dpath)
    print("\nReading statistics from file...")
    stats = gen_stats(dpath, flist)
    print("Generating table...\n")    
    stat_table = gen_table(stats)
    print(stat_table)