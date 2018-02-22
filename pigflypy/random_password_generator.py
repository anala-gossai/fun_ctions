import argparse
import random
import string


MIN_PASSWORD_LENGTH = 3
SPECIAL_CHARACTERS = "+-*/?!@#$%&"


def random_password_generator(length=9):
    """
    Generate a random password

    @param length: The length of the password
    
    @return A string password of specified length,
        with 1 number and 1 special character
    """

    if length < MIN_PASSWORD_LENGTH:
        raise ValueError(
            "password must be at least {} characters long"
            .format(MIN_PASSWORD_LENGTH)
        )

    pwd = [
        # Choose a random single digit
        str(random.randint(0, 9)),  
        # Choose a random single special character
        random.choice(SPECIAL_CHARACTERS)  
    ] + [
        random.choice(string.ascii_uppercase)
        for i in range(length - 2)
    ]

    random.shuffle(pwd)
    return ''.join(pwd)
    
    
if __name__ == '__main__':
    """
    After cd-ing into the directory with your script, 
    Run: python random_password_generator.py --length 4 
         to get a password of length 4 (for example)
    Run: python random_password_generator.py --help
         for help
    from the command line terminal 
    """
    parser = argparse.ArgumentParser('Random Password Generator')
    parser.add_argument("-l", "--length", help="the integer length of the password", default=9, type=int)
    args = parser.parse_args()
    length = args.length
    
    pwd = random_password_generator(length=length)
    print(pwd)
